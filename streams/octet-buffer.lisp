(uiop:define-package :postgres/streams/octet-buffer
  (:use :common-lisp :postgres/streams/octet-stream)
  (:export
   #:octet-buffer
   #:buffer-vector
   #:buffer-position))

(in-package :postgres/streams/octet-buffer)

(defclass octet-buffer (octet-stream)
  ((vector
    :initarg :vector
    :type (vector (unsigned-byte 8))
    :reader buffer-vector
    :initform (make-array 0
			  :element-type '(unsigned-byte 8)
			  :adjustable t
			  :fill-pointer 0)
    :documentation "The streams underlying octet-vector.")
   (position
    :initarg :position
    :type unsigned-byte
    :accessor buffer-position
    :initform 0
    :documentation "The octet-vector index for read write operations."))
  (:documentation "A memory octet-stream."))

(defmethod print-object ((self octet-buffer) stream)
  "Prints the given octet-buffer to stream."
  (print-unreadable-object (self stream :type t)
    (format stream "~A ~A"
	    (buffer-position self)
	    (buffer-vector self))))

(defmethod read-octet ((self octet-buffer) &optional (eof-error-p t) eof-value)
  "Reads an unsigned-byte 8 from the given octet-buffer."
  (with-slots (vector position) self
    (declare (type unsigned-byte position))
    (if (< position (length vector))
	(prog1 (aref vector position)
	  (incf position))
	(if (null eof-error-p)
	    eof-value
	    (error "buffer position out of range: ~A" position)))))

(defmethod read-octet-vector (length (octet-buffer octet-buffer))
  "Reads an octet-vector from the given octet-buffer.
Note that the returned vector is an array displaced to the vector
underlying octet-buffer."
  (with-slots (vector position) octet-buffer
    (prog1 (make-array length
		       :element-type '(unsigned-byte 8)
		       :displaced-to vector
		       :displaced-index-offset position)
      (incf position length))))

(defmethod write-octet ((octet integer) (octet-buffer octet-buffer))
  "Writes the given unsigned-byte 8 to octet-buffer."
  (declare (type (unsigned-byte 8) octet))
  (with-slots (vector position) octet-buffer
    (declare (type unsigned-byte position))
    (if (< position (length vector))
	(setf (aref vector position) octet)
	(vector-push-extend octet vector))
    (incf position)
    (values)))

(defmethod write-octet-vector (octet-vector (octet-buffer octet-buffer))
  "Writes the given octet-vector to octet-buffer."
  (declare (type (vector (unsigned-byte 8) octet-vector)))
  (loop for octet across octet-vector do (write-octet octet octet-buffer))
  (values))


