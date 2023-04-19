(uiop:define-package :postgres/octet-stream
    (:documentation "Functions for reading and writing octets.")
  (:use :common-lisp)
  (:export
   :octet-stream
   :read-octet
   :read-octet-vector
   :write-octet
   :write-octet-vector))

(in-package :postgres/octet-stream)

(defclass octet-stream () ()
  (:documentation "The octet-stream base class."))

(defgeneric read-octet (octet-stream &optional eof-error-p eof-value)
  (:documentation "Reads an unsigned-byte 8 from the given octet-stream.
Methods of this function must initialize eof-error-p to true.
Returns an unsigned-byte 8."))

(defgeneric read-octet-vector (length octet-stream)
  (:documentation "Reads an octet-vector from the given octet-stream.
Returns a possibly displaced octet-vector of the given length."))

(defgeneric write-octet (octet octet-stream)
  (:documentation "Writes the given unsigned-byte 8 to octet-stream.
Returns no values."))

(defgeneric write-octet-vector (octet-vector octet-stream)
  (:documentation "Writes the given octet-vector to octet-stream.
Returns no values."))
