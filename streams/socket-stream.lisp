(uiop:define-package :postgres/streams/socket-stream
    (:documentation "An octet-stream that reads and writes to a socket.")
  (:use :common-lisp)
  (:use :postgres/streams/input-buffer)
  (:use :postgres/streams/octet-stream)
  (:export :socket-stream)
  (:export :socket-stream-connect)
  (:export :socket-stream-close))

(in-package :postgres/streams/socket-stream)

(defclass socket-stream (input-buffer octet-stream)
  ((socket
    :documentation "The socket underlying this socket-stream."
    :type sb-bsd-sockets:socket
    :initform (make-instance 'sb-bsd-sockets:socket
			     :type :stream
			     :protocol :tcp)))
  (:documentation
   "An octet-stream that reads an writes to a socket."))

(defun socket-stream-connect (socket-stream host port)
  "Connects the given socket-stream to host and port.
Returns no values."
  (with-slots (socket) socket-stream
    (sb-bsd-sockets:socket-connect socket host port)
    (values)))

(defun socket-stream-close (socket-stream)
  "Closes the given socket-stream.
Returns no values."
  (with-slots (socket) socket-stream
    (sb-bsd-sockets:socket-close socket)
    (values)))

(defmethod read-available-octets (octet-vector (self socket-stream))
  "Reads the available octets into octet-vector.
Returns the number of read octets."
  (declare (type (vector (unsigned-byte 8)) octet-vector))
  (with-slots (socket) self
    (multiple-value-bind (buffer length address)
	(sb-bsd-sockets:socket-receive socket octet-vector nil)
      (declare (ignore buffer))
      (declare (ignore address))
      (values length))))

(defmethod write-octet-vector (octet-vector (self socket-stream))
  "Writes the given unsigned-byte 8 vector to octet-stream.
Returns no values."
  (declare (type (vector (unsigned-byte 8)) octet-vector))
  (with-slots (socket) self
    (sb-bsd-sockets:socket-send socket octet-vector (length octet-vector))
    (values)))









