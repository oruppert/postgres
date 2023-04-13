(uiop:define-package :konnekt/postgres/connection
  (:use :common-lisp)
  (:export
   #:connection
   #:connection-send-octet-vector
   #:connection-read-octet-vector
   #:call-with-connection))

(in-package :konnekt/postgres/connection)

(defclass connection ()
  ((output-queue
    :initarg :output-queue
    :type sb-concurrency:mailbox
    :initform (sb-concurrency:make-mailbox :name "connection-output-queue")
    :reader connection-output-queue
    :documentation "Outgoing octet-vectors are bufferd here.")
   (socket
    :initarg :socket
    :type sb-bsd-sockets:socket
    :initform (error "No socket given.")
    :reader connection-socket
    :documentation "The connection socket."))
  (:documentation
   "A connection object maintains a socket and an output-queue.
Outgoing messages (octet-vectors) are buffered in the output-queue.
If the connection was established by call-with-connection, a thread
will be started that writes the buffered messages to the connection
socket."))

(defun connection-process-output-queue (connection)
  "Receives octet vectors from the connection output-queue and writes
them to the connection socket until it receives a null value."
  (declare (type connection connection))
  (let ((socket (connection-socket connection))
	(output-queue (connection-output-queue connection)))
    (loop for octet-vector = (sb-concurrency:receive-message output-queue)
	  while octet-vector
	  do (sb-bsd-sockets:socket-send socket octet-vector nil))))

(defun connection-send-octet-vector (octet-vector connection)
  "Adds octet-vector to the output-queue of the given connection."
  (declare (type (vector (unsigned-byte 8)) octet-vector))
  (declare (type connection connection))
  (sb-concurrency:send-message (connection-output-queue connection)
			       octet-vector))

(defun connection-read-octet-vector (length connection)
  "Reads an unsigned-byte 8 vector with the given length from connection."
  (declare (type (integer 0) length))
  (declare (type connection connection))
  (sb-bsd-sockets:socket-receive (connection-socket connection) nil length
				 :element-type '(unsigned-byte 8)))

(defun call-with-connection (function &key (host #(127 0 0 1)) (port 5432))
  "Call the given function with a established connection.
The connection will be closed after the function returns.  Also starts
a thread that takes messages (octet-vectors) from the output-queue and
writes them to the connection socket."
  (declare (type function function))
  (declare (type vector host))
  (declare (type (integer 0) port))
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
			       :type :stream
			       :protocol :tcp)))
    (sb-bsd-sockets:socket-connect socket host port)
    (unwind-protect
	 (let* ((connection (make-instance 'connection :socket socket))
		;; Start writer thread.
		(writer-thread
		  (sb-thread:make-thread #'connection-process-output-queue
					 :name "connection-writer-thread"
					 :arguments connection)))
	   (unwind-protect (funcall function connection)
	     ;; Stop and join the writer-thread.  Note that the
	     ;; writer-thread will stop if it receives a nil value
	     ;; from the output-queue.
	     (let ((output-queue (connection-output-queue connection)))
	       ;; send nil
	       (sb-concurrency:send-message output-queue nil)
	       ;; join thread
	       (sb-thread:join-thread writer-thread))))
      (sb-bsd-sockets:socket-close socket))))

