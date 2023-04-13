(uiop:define-package :konnekt/postgres/network/strings
  (:use :common-lisp
	:konnekt/postgres/network/generics
	:konnekt/postgres/network/charsets)
  (:export
   #:vector-to-string
   #:string-to-vector
   #:write-string-using-charset
   #:read-string-using-charset))

(in-package :konnekt/postgres/network/strings)

#+sbcl
(defgeneric external-format (charset)
  (:method ((charset charset-us-ascii)) :us-ascii)
  (:method ((charset charset-iso-8859-1)) :latin1)
  (:method ((charset charset-utf-8)) :utf-8))

#+sbcl
(defun vector-to-string (vector charset)
  (declare (type (vector (unsigned-byte 8) *) vector))
  (declare (type charset charset))
  (let ((external-format (external-format charset)))
    (sb-ext:octets-to-string vector :external-format external-format)))

#+sbcl
(defun string-to-vector (string charset)
  (declare (type string string))
  (declare (type charset charset))
  (let ((external-format (external-format charset)))
    (sb-ext:string-to-octets string :external-format external-format)))

(defun write-string-using-charset (string output charset)
  (declare (type string string))
  (declare (type charset charset))
  (write-vector (string-to-vector string charset) output)
  (write-uint8 0 output)
  (values))

(defun read-string-using-charset (input charset)
  (declare (type charset charset))
  (let ((vector (read-null-terminated-vector input)))
    (vector-to-string vector charset)))





