(uiop:define-package :konnekt/postgres/octet-stream
  (:use :common-lisp)
  (:export #:octet-stream
	   #:read-octet
	   #:write-octet))

(in-package :konnekt/postgres/octet-stream)

(defclass octet-stream () ()
  (:documentation "The base class for all octet streams."))

(defgeneric read-octet (octet-stream &optional eof-error-p eof-value)
  (:documentation "Reads an unsigned-byte 8 from the given octet-stream."))

(defgeneric write-octet (octet octet-stream)
  (:documentation "Writes the given unsigned-byte 8 to octet-stream."))

;(defgeneric read-octet-vector (length octet-stream)
;  (:documentation "Reads an octet vector from the given octet-stream.
;The returned octet vector may be a displaced array."))



















