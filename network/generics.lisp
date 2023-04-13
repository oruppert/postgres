(uiop:define-package :konnekt/postgres/network/generics
  (:use :common-lisp)
  (:export
   #:read-uint8
   #:write-uint8
   #:read-vector
   #:write-vector
   #:read-null-terminated-vector))

(in-package :konnekt/postgres/network/generics)

(defgeneric read-uint8 (input))
(defgeneric write-uint8 (value output))

(defgeneric write-vector (vector output)
  (:method ((vector vector) output)
    (declare (type (vector (unsigned-byte 8) *) vector))
    (loop for byte across vector
	  do (write-uint8 byte output))
    (values)))

;;; The following functions may *not* return a fresh vector.
;;; So make a copy if you want to change them.
(defgeneric read-vector (length input)
  (:method ((length integer) input)
    (declare (type (integer 0) length))
    (let ((vector (make-array length :element-type '(unsigned-byte 8))))
      (loop for index below length
	    do (setf (aref vector index)
		     (read-uint8 input)))
      (values vector))))

(defgeneric read-null-terminated-vector (input)
  (:method (input)
    (loop with vector = (make-array 0
				    :element-type '(unsigned-byte 8)
				    :adjustable t
				    :fill-pointer 0)
	  for byte = (read-uint8 input)
	  until (zerop byte)
	  do (vector-push-extend byte vector)
	  finally (return vector))))











