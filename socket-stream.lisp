(uiop:define-package :postgres/socket-stream
  (:use :common-lisp
	:postgres/octet-stream)
  (:export :socket-stream
	   :socket-stream-connect
	   :socket-stream-close))

(in-package :postgres/socket-stream)

(defclass socket-stream (octet-stream)
  ((socket
    :documentation "The socket underlying this socket-stream."
    :reader socket-stream-socket
    :initform (make-instance 'sb-bsd-sockets:socket
			     :type :stream
			     :protocol :tcp)))
  (:documentation
   "An octet-stream that reads an writes to a socket."))

(defun socket-stream-connect (socket-stream host port)
  "Connects the given socket-stream to host and port.
Returns no values."
  (sb-bsd-sockets:socket-connect
   (socket-stream-socket socket-stream)
   host port)
  (values))

(defun socket-stream-close (socket-stream)
  "Closes the given socket-stream.
Returns no values."
  (sb-bsd-sockets:socket-close
   (socket-stream-socket socket-stream))
   (values))

(defmethod read-octet-vector (length (socket-stream socket-stream))
  "Reads an octet-vector from the given socket-stream."
  (sb-bsd-sockets:socket-receive
   (socket-stream-socket socket-stream)
   nil length
   :waitall t
   :element-type '(unsigned-byte 8)))






