(uiop:define-package :konnekt/postgres/octet-buffer
  (:use :common-lisp :konnekt/postgres/octet-stream)
  (:export
   #:octet-buffer
   #:buffer-vector
   #:buffer-position))

(in-package :konnekt/postgres/octet-buffer)

(defclass octet-buffer (octet-stream)
  ((vector
    :initarg :vector
    :reader buffer-vector
    :initform (make-array 0
			  :element-type '(unsigned-byte 8)
			  :adjustable t
			  :fill-pointer 0))
   (position
    :initarg :position
    :accessor buffer-position
    :initform 0))
  (:documentation "An unsigned-byte 8 buffer."))

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

(defmethod write-octet ((octet integer) (self octet-buffer))
  "Writes the given unsigned-byte 8 to octet-buffer."
  (declare (type (unsigned-byte 8) octet))
  (with-slots (vector position) self
    (declare (type unsigned-byte position))
    (if (< position (length vector))
	(setf (aref vector position) octet)
	(vector-push-extend octet vector))
    (incf position)
    (values)))
