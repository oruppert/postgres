(uiop:define-package :postgres/messages/client-message
    (:documentation "Messages send to the database server.")
  (:use :common-lisp)
  (:use :postgres/messages/message)
  (:use :postgres/streams/big-endian)
  (:use :postgres/streams/octet-buffer)
  (:use :postgres/streams/octet-stream)
  (:export :client-message)
  (:export :write-message)
  (:export :write-message-body))

(in-package :postgres/messages/client-message)

(defclass client-message (message)
  ((tag
    :documentation "The first byte of the message or null."
    :type (or character null)
    :initarg :tag
    :initform (error "No tag given.")
    :reader message-tag))
  (:documentation "The client-message base class."))

(defgeneric write-message (message stream)
  (:documentation "Writes the whole message to the given stream.
The whole message includes the messasge tag, the message length and
the message body."))

(defgeneric write-message-body (message stream)
  (:documentation ""))

(defmethod write-message ((message client-message)
			  (stream octet-stream))
  (let ((buffer (make-instance 'octet-buffer)))
    (write-message message buffer)
    (write-octet-vector (buffer-vector buffer) stream))
  (values))

(defmethod write-message ((message client-message)
			  (buffer octet-buffer))
  (when (message-tag message)
    (let ((octet (char-code (message-tag message))))
      (write-octet octet buffer)))
  (let ((start (buffer-position buffer)))
    ;; write dummy message length to preserve space
    (write-signed-byte-32 0 buffer)
    ;; write message body
    (write-message-body message buffer)
    ;; write actual message length
    (let* ((end (buffer-position buffer))
	   (actual-length (- end start)))
      (setf (buffer-position buffer) start)
      (write-signed-byte-32 actual-length buffer)
      (setf (buffer-position buffer) end)))
  (values))

(defclass startup-message (client-message)
  ((protocol-version-number
    :documentation "The protocol version number.  We support version 3."
    :initform #x30000
    :type (signed-byte 32))
   (database-user
    :documentation "The name of the database user."
    :initarg :database-user
    :type string
    :initform (error "No database-user given."))
   (database-name
    :documentation "The name of the database to connect to."
    :initarg :database-name
    :initform nil
    :type (or string null)))
  (:default-initargs
   ;; no message tag
   :tag nil)
  (:documentation
   "The startup message is the first message send by the client to the
server after a connection is established.  It is the only
client-message without message-tag."))

(defmethod send-message ((startup-message startup-message)
			 (stream octet-stream))
  "Writes the startup-message body to the given stream."
  (with-slots (protocol-version-number database-user database-name)
      startup-message
    (write-signed-byte-32 protocol-version-number stream)
    (write-utf-8-string "user" stream)
    (write-utf-8-string database-user stream)
    (unless (null database-name)
      (write-utf-8-string "database" stream)
      (write-utf-8-string database-name stream))
    (write-unsigned-byte-8 0 stream)))


