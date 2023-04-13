(uiop:define-package :konnekt/postgres/messages
  (:use :common-lisp
	:konnekt/postgres/buffer)
  (:export
   ;; Message Base Class
   #:message
   ;; Message Writing
   #:write-message-to-octet-vector
   ;; Message Reading
   #:read-octet-vector
   #:read-server-message
   ;; Message Implementations
   #:authentication-ok
   #:sync
   #:backend-key-data
   #:parameter-status
   #:command-complete
   #:ready-for-query
   #:simple-query
   #:startup-message))

(in-package :konnekt/postgres/messages)

;;; Message Base Class
(defclass message ()
  ((tag :initarg :tag))
  (:documentation "Base class for all messages."))

;;; Message Writing
(defgeneric write-message-body (message buffer charset)
  (:documentation "Writes the message body to the given buffer using charset."))

(defmethod write-message-body ((message message)
			       (buffer buffer)
			       (charset charset))
  "The default implementation of write-message does nothing."
  (declare (ignore message))
  (declare (ignore buffer))
  (declare (ignore charset)))

(defun write-message-to-octet-vector (message charset)
  "Writes message to a fresh octet vector using charset."
  (declare (type message message))
  (declare (type charset charset))
  (with-slots (tag) message
    (let ((buffer (make-buffer)))
      ;; Maybe write the message tag.
      (when tag
	(write-octet (char-code tag) buffer))
      ;; Write message length dummy.
      (write-signed-byte-32 0 buffer)
      (let ((start (buffer-position buffer)))
	;; Write message body.
	(write-message-body message buffer charset)
	;; Write real message length.
	(let* ((end (buffer-position buffer))
	       (length (- end start)))
	  (setf (buffer-position buffer) start)
	  (write-signed-byte-32 length buffer)))
      ;; And return the octet vector.
      (values (buffer-vector buffer)))))

;;; Message Reading
(defgeneric read-octet-vector (port length &key wait)
  (:documentation "Reads an octet vector with the given length from port.
If wait is true, waits until enough bytes are available,
otherwise returns null."))















;;(defgeneric read-octet (port))
;;(defgeneric read-octet-vector (port length &key wait))

;;(defgeneric write-octet (octet port))
;;(defgeneric write-octet-vector (vector port))

(defgeneric parse-server-message (tag buffer charset)
  (:documentation "Parses the server message for the given tag from buffer."))

(defun read-server-message (port charset)
  "Reads a server message from the given port using charset.
This function may return nil if not enough data is available."
  (let ((header (read-octet-vector port 5 :wait nil)))
    (when header
      (let* ((buffer (make-buffer header))
	     (tag (read-octet buffer))
	     (length (- (read-signed-byte-32 buffer) 4))
	     (body (read-octet-vector port length :wait nil)))
	(parse-server-message (code-char tag)
			      (make-buffer body)
			      charset)))))

;;; Message Implementations
(defclass authentication (message) ())
(defclass authentication-ok (authentication) ())

(defmethod parse-server-message ((tag (eql #\R))
				 (buffer buffer)
				 (charset charset))
  (ecase (read-signed-byte-32 buffer)
    (0 (make-instance 'authentication-ok))))

(defclass sync (message) ()
  (:default-initargs :tag #\S))

(defclass backend-key-data (message) ())

(defmethod parse-server-message ((tag (eql #\K))
				 (buffer buffer)
				 (charset charset))
  (make-instance 'backend-key-data))

(defclass parameter-status (message)
  ((name :initarg :name :reader parameter-status-name)
   (value :initarg :value :reader parameter-status-value)))

(defmethod print-object ((self parameter-status) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A: ~A"
	    (parameter-status-name self)
	    (parameter-status-value self))))

(defmethod parse-server-message ((message-tag (eql #\S))
				 (buffer buffer)
				 (charset charset))
  (make-instance 'parameter-status
		 :name (read-string-using-charset buffer charset)
		 :value (read-string-using-charset buffer charset)))

(defclass command-complete (message)
  ((command-tag :initarg :command-tag)))

(defmethod print-object ((self command-complete) stream)
  (print-unreadable-object (self stream :type t)
    (with-slots (command-tag) self
      (format stream "~A" command-tag))))

(defmethod parse-server-message ((tag (eql #\C))
				 (buffer buffer)
				 (charset charset))
  (let ((command-tag (read-string-using-charset buffer charset)))
    (make-instance 'command-complete :command-tag command-tag)))

(defclass ready-for-query (message) ())

(defmethod parse-server-message ((tag (eql #\Z))
				 (buffer buffer)
				 (charset charset))
  (make-instance 'ready-for-query))

(defclass simple-query (message)
  ((string :initarg :string))
  (:default-initargs :tag #\Q))

(defmethod write-message-body ((self simple-query)
			       (buffer buffer)
			       (charset charset))
  (with-slots (string) self
    (write-string-using-charset string buffer charset)))

(defclass startup-message (message)
  ((protocol-version-number :initform 196608)
   (user :initarg :user)
   (database :initarg :database))
  (:default-initargs :tag nil))

(defmethod write-message-body ((self startup-message)
			       (buffer buffer)
			       (charset charset))
  (with-slots (protocol-version-number user database) self
    (write-signed-byte-32 protocol-version-number buffer)
    (write-string-using-charset "user" buffer charset)
    (write-string-using-charset user buffer charset)
    (unless (null database)
      (write-string-using-charset "database" buffer charset)
      (write-string-using-charset database buffer charset))
    (write-octet 0 buffer)))

#+nil
(defclass parameter-description (message)
  ((types :initarg :types :reader parameter-types)))

#+nil
(defmethod read-message-body ((message-tag (eql #\t)) stream)
  (make-instance 'parameter-description-message
		 :types  (loop repeat (read-int32 stream)
			       collect (read-int32 stream))))

(defclass row-description (message)
  ((fields :initarg :fields :reader row-description-fields)))

(defclass field ()
  ((name :initarg :name :reader field-name)
   (table-oid :initarg :table-oid :reader field-table-oid)
   (column-index :initarg :column-index :reader field-column-index)
   (type-id :initarg :type-id :reader field-type-id)
   (data-type-size :initarg :data-type-size :reader field-data-type-size)
   (type-modifier :initarg :type-modifier :reader field-type-modifier)
   (format-code :initarg :format-code :reader field-format-code)))

(defmethod parse-message-body ((tag (eql #\T))
			       (buffer buffer)
			       (charset charset))
  (make-instance
   'row-description
   :fields (loop repeat (read-signed-byte-16 buffer)
		 collect
		 (make-instance 'field
				:name (read-string-using-charset buffer charset)
				:table-oid (read-signed-byte-16 buffer)
				:column-index (read-signed-byte-16 buffer)
				:type-id (read-signed-byte-32 buffer)
				:data-type-size (read-signed-byte-16 buffer)
				:type-modifier (read-signed-byte-16 buffer)
				:format-code (read-signed-byte-16 buffer)))))

(defclass data-row (message)
  ((values :initarg :values :reader data-row-values)
   (charset :initarg :charset :reader data-row-charset)))

#+nil
;;(defmethod read-message-body ((tag (eql #\D)) input charset)
;;  (make-instance
;;   'data-row
;;   :charset charset
;;   :values (loop repeat (read-signed-byte-16 input)
;;		 for length = (read-signed-byte-32 input)
;;		 collect (if (= -1 length)
;;			     nil
;;			     (loop with vector = (make-array length
;;							     :element-type '(unsigned-byte-8
;;			     (read-vector length input)))))

(defclass command-complete (message)
  ((command-tag :initarg :command-tag :reader command-complete-command-tag)))

(defmethod print-object ((self command-complete) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A" (command-complete-command-tag self))))

(defmethod read-message-body ((tag (eql #\C)) input charset)
  (make-instance
   'command-complete
   :command-tag (read-string-using-charset input charset)))

(defclass parse-message (message)
  ((name :initarg :name)
   (string :initarg :string)
   (types :initarg :types))
  (:default-initargs :tag #\P
		     :name ""
		     :types nil))

(defmethod write-message ((self parse-message) output charset)
  (with-slots (name string types) self
    (write-string-using-charset name output charset)
    (write-string-using-charset string output charset)
    (write-signed-byte-16 (length types) output)
    (dolist (type types)
      (write-signed-byte-32 type output))))

(defclass error-response (message)
  ((fields :initarg :fields)))

(defmethod print-object ((self error-response) stream)
  (print-unreadable-object (self stream :type t)
    (with-slots (fields) self
      (format stream "~a" fields))))

(defmethod read-message-body ((tag (eql #\E)) input charset)
  (make-instance 'error-response
		 :fields (loop for byte = (read-uint8 input)
			       until (zerop byte)
			       collect (cons (code-char byte)
					     (read-string-using-charset input charset)))))

(defclass bind-message (message)
  ((portal :initarg :portal :reader bind-portal)
   (source :initarg :source :reader bind-source))
  (:default-initargs :tag #\B))

(defmethod write-message ((self bind-message) output charset)
  (with-slots (portal source) self
    (write-string-using-charset portal output charset)
    (write-string-using-charset source output charset)
    (write-signed-byte-16 0 output)
    (write-signed-byte-16 0 output)
    (write-signed-byte-16 0 output)))


(defclass bind-complete (message) ())

(defmethod read-message-body ((tag (eql #\2)) output charset)
  (make-instance 'bind-complete))

(defclass parse-complete (message) ())

(defmethod read-message-body ((tag (eql #\1)) output charset)
  (make-instance 'parse-complete))

(defclass execute-message (message)
  ((portal :initarg :portal)
   (row-count :initarg :row-count))
  (:default-initargs :tag #\E :row-count 0))

(defmethod write-message ((self execute-message) output charset)
  (with-slots (portal row-count) self
    (write-string-using-charset portal output charset)
    (write-signed-byte-32 row-count output)))




















