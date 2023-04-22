(uiop:define-package :postgres/client-message
    (:documentation "Messages send to the database server.")
  (:use :common-lisp
	:postgres/big-endian
	:postgres/message
	:postgres/octet-buffer
        :postgres/octet-stream
	:postgres/utf-8)
  (:export :client-message
	   :startup-message))

(in-package :postgres/client-message)

(defclass client-message (message)
  ((tag
    :documentation "The first byte of the message or null."
    :type (or character null)
    :initarg :tag
    :initform (error "No tag given.")
    :reader message-tag))
  (:documentation "The client-message base class."))

(defmethod send-message :around ((message client-message)
				 (stream octet-stream))
  "Writes the whole message to the given stream.
The whole message includes the messasge tag, the message length and
the message body. Calls the primary method to write the message body.
That means primary methods must only write the message body."
  ;; We write to a buffer and then write the buffer to the given
  ;; stream.  Because we do not know the message length until we call
  ;; the primary method.
  (let ((buffer (make-instance 'octet-buffer)))
    ;; maybe write message tag
    (when (message-tag message)
      (let ((octet (char-code (message-tag message))))
	(write-octet octet buffer)))
    (let ((start (buffer-position buffer)))
      ;; write dummy message length to preserve space
      (write-signed-byte-32 0 buffer)
      ;; write message body
      (call-next-method message buffer)
      ;; write actual message length
      (let* ((end (buffer-position buffer))
	     (actual-length (- end start)))
	(setf (buffer-position buffer) start)
	(write-signed-byte-32 actual-length buffer)
	(setf (buffer-position buffer) end)))
    ;; write buffer to the given stream
    (write-octet-vector (buffer-vector buffer) stream))
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


