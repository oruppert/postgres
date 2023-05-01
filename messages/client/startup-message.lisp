(uiop:define-package :postgres/messages/client/startup-message
    (:documentation "Postgres wire protocol startup message")
  (:use :common-lisp)
  (:use :postgres/messages/message)
  (:use :postgres/messages/client-message)
  (:export :startup-message))

(in-package :postgres/messages/client/startup-message)

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


#+nil
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


