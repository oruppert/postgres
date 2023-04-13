(uiop:define-package :konnekt/postgres/network/buffer
  (:use :common-lisp
	:konnekt/postgres/network/generics)
  (:export
   #:make-buffer
   #:buffer-vector
   #:buffer-position))

(in-package :konnekt/postgres/network/buffer)

(defstruct (buffer (:constructor make-buffer (&optional vector position)))
  (vector (make-array 0
		      :element-type '(unsigned-byte 8)
		      :adjustable t
		      :fill-pointer 0)
   :type (vector (unsigned-byte 8) *))
  (position 0 :type (integer 0)))

(defmethod read-uint8 ((buffer buffer))
  (prog1 (aref (buffer-vector buffer)
	       (buffer-position buffer))
      (incf (buffer-position buffer))))

(defmethod write-uint8 (value (buffer buffer))
  (declare (type (unsigned-byte 8) value))
  (if (< (buffer-position buffer)
	 (length (buffer-vector buffer)))
      (setf (aref (buffer-vector buffer)
		  (buffer-position buffer))
	    value)
      (vector-push-extend value (buffer-vector buffer)))
  (incf (buffer-position buffer))
  (values))

(defmethod read-vector ((length integer)
			(buffer buffer))
  (prog1 (make-array length
		     :element-type '(unsigned-byte 8)
		     :displaced-to (buffer-vector buffer)
		     :displaced-index-offset (buffer-position buffer))
    (incf (buffer-position buffer) length)))

(defmethod read-null-terminated-vector ((buffer buffer))
  (loop for length from 0
	for index = (+ length (buffer-position buffer))
	for byte = (aref (buffer-vector buffer) index)
	until (zerop byte)
	finally (return
		  (prog1
		      (make-array length
				  :element-type '(unsigned-byte 8)
				  :displaced-to (buffer-vector buffer)
				  :displaced-index-offset (buffer-position buffer))
		    (incf (buffer-position buffer) length)
		    (incf (buffer-position buffer))))))








