(uiop:define-package :postgres/messages/messages
  (:use :common-lisp
	:postgres/network/buffer
	:postgres/network/charsets
	:postgres/network/integers
	:postgres/network/sockets
	:postgres/network/generics
	:postgres/network/strings)
  (:export
   ;; Message Base Class
   #:message
   ;; Message IO
   #:write-message
   #:read-message
   ;; Messages
   #:authentication
   #:authentication-ok
   #:sync
   #:backend-key-data
   #:parameter-status
   #:parameter-status-name
   #:parameter-status-value
   #:command-complete
   #:ready-for-query
   #:simple-query
   #:startup-message
   #:parameter-description
   #:parameter-types
   #:row-description
   #:field
   #:row-description-fields
   #:field-name
   #:field-table-oid
   #:field-column-index
   #:field-type-id
   #:field-data-type-size
   #:field-type-modifier
   #:field-format-code
   #:data-row
   #:data-row-values
   #:command-complete-command-tag
   #:data-row-charset
   #:parse-message
   #:error-response
   #:bind-message
   #:parse-complete
   #:execute-message
   #:bind-complete))

(in-package :postgres/messages/messages)

;;; Message
(defclass message ()
  ((tag :initarg :tag)))

;;; Message Writing

(defgeneric write-message (message output charset))

(defmethod write-message ((message message) socket (charset charset))
  (declare (ignore message))
  (declare (ignore socket))
  (declare (ignore charset)))

(defmethod write-message :around ((message message) socket (charset charset))
  (with-slots (tag) message
    (let ((offset (if tag 1 0))
	  (buffer (make-buffer)))
      (when tag
	(write-uint8 (char-code tag) buffer))
      (write-int32 0 buffer)
      (call-next-method message buffer charset)
      (setf (buffer-position buffer) offset)
      (write-int32 (- (length (buffer-vector buffer)) offset) buffer)
      (write-vector (buffer-vector buffer) socket)
      (values))))

;;; Message Reading

(defgeneric read-message (input charset))
(defgeneric read-message-body (tag input charset))

(defmethod read-message (socket charset)
  (let ((buffer (make-buffer (read-vector 5 socket))))
    (let ((tag (code-char (read-uint8 buffer)))
	  (len (- (read-int32 buffer) 4)))
      (let ((buffer (make-buffer (read-vector len socket))))
	(read-message-body tag buffer charset)))))

;;; Messages
(defclass authentication (message) ())
(defclass authentication-ok (authentication) ())

(defmethod read-message-body ((tag (eql #\R)) input charset)
  (ecase (read-int32 input)
    (0 (make-instance 'authentication-ok))))

(defclass sync (message) ()
  (:default-initargs :tag #\S))

(defclass backend-key-data (message) ())

(defmethod read-message-body ((tag (eql #\K)) input charset)
  (make-instance 'backend-key-data))

(defclass parameter-status (message)
  ((name :initarg :name :reader parameter-status-name)
   (value :initarg :value :reader parameter-status-value)))

(defmethod print-object ((self parameter-status) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A: ~A"
	    (parameter-status-name self)
	    (parameter-status-value self))))

(defmethod read-message-body ((message-tag (eql #\S)) input charset)
  (make-instance 'parameter-status
		 :name (read-string-using-charset input charset)
		 :value (read-string-using-charset input charset)))

(defclass command-complete (message)
  ((command-tag :initarg :command-tag)))

(defmethod print-object ((self command-complete) stream)
  (print-unreadable-object (self stream :type t)
    (with-slots (command-tag) self
      (format stream "~A" command-tag))))

(defmethod read-message-body ((tag (eql #\C)) input charset)
  (make-instance 'command-complete
		 :command-tag (read-string-using-charset input charset)))

(defclass ready-for-query (message) ())

(defmethod read-message-body ((tag (eql #\Z)) input charset)
  (make-instance 'ready-for-query))

(defclass simple-query (message)
  ((string :initarg :string))
  (:default-initargs :tag #\Q))

(defmethod write-message ((self simple-query) output charset)
  (with-slots (string) self
    (write-string-using-charset string output charset)))

(defclass startup-message (message)
  ((protocol-version-number :initform 196608)
   (user :initarg :user)
   (database :initarg :database))
  (:default-initargs :tag nil))

(defmethod write-message ((self startup-message) output charset)
  (with-slots (protocol-version-number user database) self
    (write-int32 protocol-version-number output)
    (write-string-using-charset "user" output charset)
    (write-string-using-charset user output charset)
    (unless (null database)
      (write-string-using-charset "database" output charset)
      (write-string-using-charset database output charset))
    (write-uint8 0 output)))

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

(defmethod read-message-body ((tag (eql #\T)) input charset)
  (make-instance
   'row-description
   :fields (loop repeat (read-int16 input)
		 collect
		 (make-instance 'field
				:name (read-string-using-charset input charset)
				:table-oid (read-int32 input)
				:column-index (read-int16 input)
				:type-id (read-int32 input)
				:data-type-size (read-int16 input)
				:type-modifier (read-int32 input)
				:format-code (read-int16 input)))))

(defclass data-row (message)
  ((values :initarg :values :reader data-row-values)
   (charset :initarg :charset :reader data-row-charset)))

(defmethod read-message-body ((tag (eql #\D)) input charset)
  (make-instance
   'data-row
   :charset charset
   :values (loop repeat (read-int16 input)
		 for length = (read-int32 input)
		 collect (if (= -1 length)
			     nil
			     (read-vector length input)))))

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
    (write-int16 (length types) output)
    (dolist (type types)
      (write-uint32 type output))))

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
    (write-int16 0 output)
    (write-int16 0 output)
    (write-int16 0 output)))

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
    (write-int32 row-count output)))




















