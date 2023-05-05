(uiop:define-package :postgres/streams/socket-stream
    (:documentation "An octet-stream that reads and writes to a socket.")
  (:use :common-lisp)
  (:use :postgres/streams/octet-stream)
  (:export :socket-stream)
  (:export :socket-stream-connect)
  (:export :socket-stream-close))

(in-package :postgres/streams/socket-stream)

(defclass socket-stream (octet-stream)
  ((socket
    :documentation "The streams underlying socket."
    :type sb-bsd-sockets:socket
    :initform (make-instance 'sb-bsd-sockets:socket
			     :type :stream
			     :protocol :tcp))
   (input-buffer
    :documentation ""
    :type (vector (unsigned-byte 8))
    :initform (make-array #x1000
			  :element-type '(unsigned-byte 8)
			  :fill-pointer 0))
   (buffer-position
    :documentation "The current input-buffer position."
    :type (integer 0)
    :initform 0))
  (:documentation
   "An octet-stream that reads an writes to a socket.
Input is buffered."))

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

(defmethod read-octet ((self input-buffer) &optional (eof-error-p t) eof-value)
  ""
  (declare (ignore eof-error-p))
  (declare (ignore eof-value))
  (with-slots (socket input-buffer buffer-position) self
    ;; Ensure buffer contains data.
    (when (= buffer-position (fill-pointer input-buffer))
      (setf buffer-position 0)
      (setf (fill-pointer input-buffer)
	    (array-total-size input-buffer))
      (loop
	do
	   (setf (fill-pointer input-buffer)
		 (array-total-size input-buffer))
	   (setf (fill-pointer input-buffer)
		 (multiple-value-bind (buffer length address)
		     (sb-bsd-sockets:socket-receive
		      socket input-buffer nil)
		   (declare (ignore buffer))
		   (declare (ignore address))
		   (values length)))
	while (zerop (fill-pointer input-buffer))))
    ;; Return the current octet and increment position.
    (prog1 (aref input-buffer buffer-position)
      (incf buffer-position))))


(defmethod write-octet-vector (octet-vector (self socket-stream))
  "Writes octet-vector to the given socket-stream.
Returns no values."
  (declare (type (vector (unsigned-byte 8)) octet-vector))
  (with-slots (socket) self
    (sb-bsd-sockets:socket-send socket octet-vector nil)
    (values)))









