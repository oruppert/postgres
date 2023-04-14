(uiop:define-package :postgres/streams/octet-stream
  (:use :common-lisp)
  (:export #:octet-stream
	   #:read-octet
	   #:read-octet-vector
	   #:write-octet
	   #:write-octet-vector))

(in-package :postgres/streams/octet-stream)

(defclass octet-stream () ()
  (:documentation "The base class for all octet streams."))

(defgeneric read-octet (octet-stream &optional eof-error-p eof-value)
  (:documentation "Reads an unsigned-byte 8 from the given octet-stream."))

(defgeneric read-octet-vector (length octet-stream)
  (:documentation "Reads an octet-vector from the given octet-stream."))

(defgeneric write-octet (octet octet-stream)
  (:documentation "Writes the given unsigned-byte 8 to octet-stream."))

(defgeneric write-octet-vector (octet-vector octet-stream)
  (:documentation "Writes the given octet-vector to octet-stream."))




















