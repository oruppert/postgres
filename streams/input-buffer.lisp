(uiop:define-package :postgres/streams/input-buffer
    (:documentation "")
  (:use :common-lisp)
  (:use :postgres/streams/octet-stream))

(in-package :postgres/streams/input-buffer)

(defclass input-buffer ()
  ((vector
    :documentation ""
    :type (vector (unsigned-byte 8))
    :initform (make-array #x1000 :element-type '(unsigned-byte 8)))
   (position
    :documentation ""
    :type (integer 0)
    :initform 0)
   (fill-pointer
    :documentation ""
    :type (integer 0)
    :initform 0))
  (:documentation ""))

(defmethod read-octet ((self input-buffer) &optional (eof-error-p t) eof-value)
  ""
  (declare (ignore eof-error-p))
  (declare (ignore eof-value))
  (with-slots (vector position fill-pointer) self
    (when (= position fill-pointer)
      (setf position 0)
      (loop do (setf fill-pointer
		     (read-available-octets vector self))
	    while (zerop fill-pointer)))
    (prog1 (aref vector position)
      (incf position))))

(defmethod read-octet-vector (length (self input-buffer))
  "Returns an octet-vector of the given length using read-octet."
  (loop with result = (make-array length :element-type '(unsigned-byte 8))
	for index from 0 below length
	do (setf (aref result index) (read-octet self))
	finally (return result)))



