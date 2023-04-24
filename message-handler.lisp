(uiop:define-package :postgres/message-handler
    (:documentation "")
  (:use :common-lisp
	:postgres/message)
  (:export))

(in-package :postgres/message-handler)

(defclass message-handler ()
  ((message-processing-loop-running-p
    :documentation "True if this handler wants to process messages."
    :type boolean
    :initform nil
    :accessor message-processing-loop-running-p))
  (:documentation "Handles messages."))

(defgeneric process-message (database handler message)
  (:documentation "Process the given message.
Repeatatly called by message-processing-loop until
stop-message-processing-loop is called by a method."))

(defun stop-message-processing-loop (message-handler)
  "Stop message processing."
  (setf (message-handler-running-p message-handler) nil))


(defun message-processing-loop (database handler)
  ""
  (setf (message-handler-running-p message-handler) t)
  (loop while (message-handler-running-p message-handler)
	for message = (read-message database)
	do (process-message database handler message)))










