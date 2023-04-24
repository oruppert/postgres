(uiop:define-package :postgres/startup-handler
    (:documentation "")
  (:use :common-lisp)
  (:use :postgres/message-handler))

(in-package :postgres/startup-handler)


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

