(uiop:define-package :postgres/message-handler
    (:documentation "")
  (:use :common-lisp
	:postgres/message)
  (:export))

(in-package :postgres/message-handler)

(defgeneric process-messages-p (handler)
  (:documentation "True if handler wants to process messages."))

;(defgeneric (setf process-messages-p) (boolean handler)
;  (:documentation "True if handler wants to process messages."))

(defclass message-handler ()
  ((running-p
    :documentation "True if this handler want to process more messages.
Initialized by the message-loop function.  Set this value to nil in
handle-server-message to stop further message processing."
    :type boolean
    :accessor message-handler-running-p))
  (:documentation
   "Responsible for handling messages send by the database-server."))

(defgeneric handle-server-message (database message-handler message)
  (:documentation "Handles the given message.
This method will be called by message-loop in a loop until
message-handler-running-p returns false."))


(defun message-loop (database message-handler)
  ""
  (declare (type message-handler message-handler))
  (setf (message-handler-running-p message-handler) t)
  (loop while (message-handler-running-p message-handler)
	for message = (read-message database)
	do (handle-server-message database message-handler message)))


#+nil
(defclass startup-handler (message-handler) ())

#+nil
(defmethod handle-server-message (database
				  (startup-handler startup-handler)
				  (server-message authentication-ok))
  "Handles a authentication-ok message."
  (declare (ignore database))
  (declare (ignore server-message)))

#+nil
(defmethod handle-server-message (database
				  (startup-handler startup-handler)
				  (ready-for-query ready-for-query ))
  "Handles a authentication-ok message."
  (declare (ignore database))
  (declare (ignore ready-for-query))
  (setf (message-handler-running startup-handler) nil))

#+nil
(defclass simple-query-handler (message-handler) ())









