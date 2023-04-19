(uiop:define-package :postgres/octet-buffer
  (:use :common-lisp
	:postgres/octet-stream)
  (:export :octet-buffer
	   :buffer-vector
	   :buffer-position))

(in-package :postgres/octet-buffer)

(defclass octet-buffer (octet-stream)
  ((vector
    :documentation "The underlying vector."
    :initarg :vector
    :type (vector (unsigned-byte 8))
    :reader buffer-vector
    :initform (make-array 0
			  :element-type '(unsigned-byte 8)
			  :adjustable t
			  :fill-pointer 0))
   (position
    :documentation "The current vector position."
    :initarg :position
    :type unsigned-byte
    :accessor buffer-position
    :initform 0))
  (:documentation
   "An octet-stream that reads and writes to a vector."))

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
  "Reads a vector from the given octet-buffer."
  (with-slots (vector position) octet-buffer
    (prog1 (make-array length
		       :element-type '(unsigned-byte 8)
		       :displaced-to vector
		       :displaced-index-offset position)
      (incf position length))))

(defmethod write-octet (octet (octet-buffer octet-buffer))
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


