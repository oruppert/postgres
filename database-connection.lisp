(uiop:define-package :postgres/database-connection
  (:use :common-lisp
	:postgres/message
	:postgres/socket-stream))

(in-package :postgres/database-connection)

(defclass database-connection ()
  ((socket-stream :initform (make-instance 'socket-stream))
   (output-mailbox :initform (sb-concurrency:make-mailbox))
   (output-thread)))

(defun output-loop (database-connection)
  "Writes messages from the output-queue to the socket-stream."
  (with-slots (socket-stream output-queue) database-connection
    (loop for message = (sb-concurrency:receive-message output-queue)
	  until (null message)
	  do (send-message message socket-stream))))

(defun database-connection-start-output-thread (database-connection)
  (with-slots (output-thread) database-connection
    (setf output-thread
	  (sb-thread:make-thread #'output-loop
				 :arguments database-connection))))




(defmethod send-message (message (self database-connection))
  (with-slots (output-mailbox) self
    (sb-concurrency:send-message output-mailbox message)))

(defmethod read-message ((self database-connection))
  (with-slots (input-mailbox) self
    (sb-concurrency:receive-message input-mailbox)))

#+nil
(defmethod send-message ((client-message client-message)
			 (database-connection database-connection))
  "Appends the given message to the output queue of database-connection.")







