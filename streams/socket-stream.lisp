(uiop:define-package :postgres/streams/socket-stream
  (:use :common-lisp :postgres/streams/octet-stream)
  (:export :socket-stream
	   :call-with-connected-socket-stream))

(in-package :postgres/streams/socket-stream)

(defclass socket-stream (octet-stream)
  ((socket
    :initarg :socket
    :type sb-bsd-sockets:socket
    :initform (error "No socket given.")
    :documentation "The socket underlying this stream."))
  (:documentation "An octet-stream that reads and writes to a socket."))

(defun call-with-connected-socket-stream (function &key host port)
  "Calls function with a socket-stream connected to host and port.
Closes the socket-stream connection after the function returns."
  (declare (type (function (socket-stream)) function))
  (declare (type vector host))
  (declare (type (integer 0) port))
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
			       :type :stream
			       :protocol :tcp)))
    (sb-bsd-sockets:socket-connect socket host port)
    (unwind-protect
	 (funcall function (make-instance 'socket-stream :socket socket))
      (sb-bsd-sockets:socket-close socket))))

(defmethod read-octet ((self socket-stream) &optional eof-error-p eof-value)
  "Reads an unsigned-byte 8 from the given socket-stream."
  (declare (ignore eof-error-p))
  (declare (ignore eof-value))
  (aref (read-octet-vector 1 self) 0))

(defmethod read-octet-vector (length (self socket-stream))
  "Reads an octet-vector from the given socket-stream."
  (with-slots (socket) self
    (sb-bsd-sockets:socket-receive socket nil length
				   :waitall t
				   :element-type '(unsigned-byte 8))))

(defmethod write-octet (octet (socket-stream socket-stream))
  "Writes the given unsigned-byte 8 to socket-stream."
  (write-octet-vector
   (make-array 1 :element-type '(unsigned-byte 8) :initial-element octet)
   socket-stream))

(defmethod write-octet-vector (octet-vector (socket-stream socket-stream))
  "Writes octet-vector to the given socket-stream."
  (declare (type (vector (unsigned-byte 8)) octet-vector))
  (with-slots (socket) socket-stream
    (sb-bsd-sockets:socket-send socket octet-vector nil))
  (values))

