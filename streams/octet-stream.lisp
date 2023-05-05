(uiop:define-package :postgres/streams/octet-stream
    (:documentation "Functions for reading and writing octets.")
  (:use :common-lisp)
  (:export :octet-stream-eof)
  (:export :read-octet)
  (:export :read-octet-vector)
  (:export :write-octet)
  (:export :write-octet-vector)
  (:export :octet-stream))

(in-package :postgres/streams/octet-stream)

(define-condition octet-stream-eof () ()
  (:documentation "Thrown when the end of an octet-stream is reached."))

(defgeneric read-octet (stream &optional eof-error-p eof-value)
  (:documentation "Reads an unsigned-byte 8 from stream.
Methods of this function must initialize eof-error-p to true.
Returns an unsigned-byte 8."))

(defgeneric read-octet-vector (length stream)
  (:documentation "Reads an unsigned-byte 8 vector from stream.
Returns a possibly displaced vector of the given length."))

(defgeneric write-octet (octet stream)
  (:documentation "Writes the given unsigned-byte 8 to stream.
Returns no values."))

(defgeneric write-octet-vector (octet-vector stream)
  (:documentation "Writes the given unsigned-byte 8 vector to stream."))

(defclass octet-stream () ()
  (:documentation "Abstract octet-stream base class.
Implements read-octet-vector and write-octet-vector in terms of
read-octet and write-octet."))

(defmethod read-octet-vector (length (octet-stream octet-stream))
  "Reads an unsigned-byte 8 vector from octet-stream using read-octet."
  (declare (type (integer 0) length))
  (loop for result = (make-array length :element-type '(unsigned-byte 8))
	for index from 0 below length
	do (setf (aref result index)
		 (read-octet octet-stream))
	finally (return result)))

(defmethod write-octet-vector (octet-vector (octet-stream octet-stream))
  "Writes the given octet-vector to octet-stream using write-octet."
  (declare (type (vector (unsigned-byte 8)) octet-vector))
  (loop for octet across octet-vector
	do (write-octet octet octet-stream))
  (values))

