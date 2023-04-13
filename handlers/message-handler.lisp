(uiop:define-package :konnekt/postgres/handlers/message-handler
  (:use :common-lisp
	:konnekt/postgres/messages/messages)
  (:export
   #:message-handler
   #:handle-message
   #:abort-message-loop
   #:message-loop))

(in-package :konnekt/postgres/handlers/message-handler)

(defclass handler ()
  ((running :initform nil :accessor hander-running)))


(defclass message-handler () ())

(defgeneric handle-message (connection message-handler message))

(defun abort-message-loop (&rest values)
  (throw 'message-loop-done values))

#+nil
(defun message-loop (connection message-handler)
  (values-list
   (catch 'message-loop-done
     (loop with stream = (connection-socket connection)
	   for charset = (connection-charset connection)
	   for message = (read-message stream charset)
	   do (handle-message connection message-handler message)))))

