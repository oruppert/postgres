 (uiop:define-package :postgres/client-message
  (:use :common-lisp
        :postgres/octet-buffer
        :postgres/octet-stream)
  (:export))


(in-package :konnekt/postgres/client-message)

#+nil
(defclass client-message ()
  ((message-tag
    :initarg :message-tag
    :initform (error "No message tag given.")
    :type (or character null)
    :reader client-message-tag
    :documentation "The us-ascii character representing the first
octet of the message.  Will be ignored if null."))
  (:documentation "Base class for all postgres client messages."))

#+nil
(defgeneric send-client-message (client-message output)
  (:documentation "Sends client-message to the given output."))

#+nil
(defgeneric write-message-body (message octet-stream)
  (:documentation "Writes the message body to octet-stream."))

#+nil
(defgeneric write-client-message-body
    (client-message octet-stream char-encoding)
  (:documentation
   "Writes the client-message body to octet-stream using char-encoding."))

#+nil
(defun write-client-message-to-octet-vector (client-message char-encoding)
  "Writes client-message to a fresh octet vector.
Message strings will be encoded using the given char-encoding.  The
resulting octet vector contains the whole message, including the
message-tag (if any), the size of the message and the message-body."
  (declare (type client-message client-message))
  (declare (type char-encoding char-encoding))
  (let ((buffer (make-instance 'octet-buffer)))
    ;; maybe write message tag
    (unless (null (client-message-tag client-message))
      (write-char-using-encoding (client-message-tag client-message) buffer
				 (make-instance 'char-encoding-us-ascii)))
    (let ((start (buffer-position buffer)))
      ;; write dummy length to preserve space
      (write-signed-byte-32 0 buffer)
      ;; write the message body
      (write-client-message-body client-message buffer char-encoding)
      ;; overwrite the dummy length with the actual length
      (let* ((end (length (buffer-vector buffer)))
	     (actual-length (- end start)))
	(setf (buffer-position buffer) start)
	(write-signed-byte-32 actual-length buffer)))
    ;; return the octet-vector of buffer
    (buffer-vector buffer)))

#+nil
(defclass startup-message (client-message)
  ((protocol-version-number
    :initform #x30000
    :type (signed-byte 32)
    :documentation "The protocol version number.  We support version 3.")
   (database-user
    :initarg :database-user
    :type string
    :initform (error "No database-user given.")
    :documentation "The name of the database user.")
   (database-name
    :initarg :database-name
    :initform nil
    :type (or string null)
    :documentation "The name of the database.
Defaults to database-user if not given."))
  (:default-initargs
   ;; no message tag
   :message-tag nil)
  (:documentation
   "The startup message is the first message send by the client to the
server after a connection is established.  It is the only
client-message without message-tag."))

#+nil
(defmethod write-client-message-body ((self startup-message)
				      (octet-stream octet-stream)
				      (char-encoding char-encoding))
  "Writes the message body of the given startup-message to octet-stream."
  (with-slots (protocol-version-number database-user database-name) self
    (write-signed-byte-32 protocol-version-number octet-stream)
    (write-string-using-encoding "user" octet-stream char-encoding)
    (write-string-using-encoding database-user octet-stream char-encoding)
    (unless (null database-name)
      (write-string-using-encoding "database" octet-stream char-encoding)
      (write-string-using-encoding database-name octet-stream char-encoding))
    (write-unsigned-byte-8 0 octet-stream)))

#+nil
(defclass simple-query (client-message)
  ((string
    :initarg :string
    :type string
    :reader simple-query-string
    :documentation "The sql string to execute by the database server."))
  (:default-initargs :message-tag #\Q)
  (:documentation "A simple-query message."))

#+nil
(defmethod write-client-message-body ((simple-query simple-query)
				      (octet-stream octet-stream)
				      (char-encoding char-encoding))
  "Writes the message body of the given simple-query to octet-stream."
  (write-string-using-encoding (simple-query-string simple-query)
			       octet-stream
			       char-encoding))

#+nil
(defclass sync-message (client-message) ()
  (:default-initargs :message-tag #\S))

#+nil
(defmethod write-client-message-body ((sync-message sync-message)
				      (octet-stream octet-stream)
				      (char-encoding char-encoding))
  "Writes the message body of the given sync-message to octet-stream."
  (declare (ignore sync-message))
  (declare (ignore octet-stream))
  (declare (ignore char-encoding)))










