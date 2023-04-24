(uiop:define-package :postgres/streams/octet-stream
    (:documentation "Functions for reading and writing octets.")
  (:use :common-lisp)
  (:export :octet-stream)
  (:export :octet-stream-eof)
  (:export :read-octet)
  (:export :read-octet-vector)
  (:export :read-available-octets)
  (:export :write-octet)
  (:export :write-octet-vector))

(in-package :postgres/streams/octet-stream)

(defclass octet-stream () ()
  (:documentation "The octet-stream interface class."))

(define-condition octet-stream-eof () ()
  (:documentation "Thrown when the end of an octet-stream is reached."))

(defgeneric read-octet (octet-stream &optional eof-error-p eof-value)
  (:documentation "Reads an unsigned-byte 8 from the given octet-stream.
Methods of this function must initialize eof-error-p to true.
Returns an unsigned-byte 8."))

(defgeneric read-octet-vector (length octet-stream)
  (:documentation "Reads an unsigned-byte 8 vector from octet-stream.
Returns a possibly displaced vector of the given length."))

(defgeneric read-available-octets (octet-vector octet-stream)
  (:documentation "Reads the available octets into octet-vector.
Returns the number of read octets."))

(defgeneric write-octet (octet octet-stream)
  (:documentation "Writes the given unsigned-byte 8 to octet-stream.
Returns no values."))

(defgeneric write-octet-vector (octet-vector octet-stream)
  (:documentation "Writes the given unsigned-byte 8 vector to octet-stream.
Returns no values."))





