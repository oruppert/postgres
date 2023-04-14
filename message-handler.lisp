(uiop:define-package :konnekt/postgres/message-handler
  (:use :common-lisp
	:konnekt/postgres/server-message
	:konnekt/postgres/client-message)
  (:export
   #:message-handler
   #:message-loop
   #:handle-server-message
   #:startup-handler
   #:simple-query-handler))

(in-package :konnekt/postgres/message-handler)

(defclass message-handler ()
  ((running
    :type boolean
    :accessor message-handler-running
    :documentation "True if this handler want to process more messages.
Initialized by the message-loop function.  Set this value to nil in
handle-server-message to stop further message processing."))
  (:documentation
   "Responsible for handling messages send by the database-server."))

(defgeneric handle-server-message (database message-handler server-message)
  (:documentation "Handles the given server message.
This method will be called by message-loop in a loop until the running
slot of message-handler is false."))

(defun message-loop (database message-handler)
  ""
  (declare (type message-handler message-handler))
  (with-slots (running) message-handler
    (setf running t)
    (loop while running
	  for server-message = (read-server-message database)
	  do (handle-server-message database message-handler
				    server-message))))

(defclass startup-handler (message-handler) ())

(defmethod handle-server-message (database
				  (startup-handler startup-handler)
				  (server-message authentication-ok))
  "Handles a authentication-ok message."
  (declare (ignore database))
  (declare (ignore server-message)))

(defmethod handle-server-message (database
				  (startup-handler startup-handler)
				  (ready-for-query ready-for-query ))
  "Handles a authentication-ok message."
  (declare (ignore database))
  (declare (ignore ready-for-query))
  (setf (message-handler-running startup-handler) nil))

(defclass simple-query-handler (message-handler) ())









