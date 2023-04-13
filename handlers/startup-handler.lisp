(uiop:define-package :konnekt/postgres/handlers/startup-handler
  (:use :common-lisp
	:konnekt/postgres/handlers/message-handler
	:konnekt/postgres/messages/authentication-message
	:konnekt/postgres/messages/ready-for-query-message
	:konnekt/postgres/io/connection)
  (:export :startup-handler))

(in-package :konnekt/postgres/handlers/startup-handler)

(defclass startup-handler (message-handler) ())

(defmethod handle-message ((connection connection)
			   (handler startup-handler)
			   (message authentication-ok)))

(defmethod handle-message ((connection connection)
			   (handler startup-handler)
			   (message ready-for-query-message))
  (abort-message-loop))

