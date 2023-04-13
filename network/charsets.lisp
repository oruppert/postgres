(uiop:define-package :konnekt/postgres/network/charsets
    (:use :common-lisp)
  (:export
   #:charset
   #:charset-us-ascii
   #:charset-iso-8859-1
   #:charset-utf-8
   #:charset-for-name))

(in-package :konnekt/postgres/network/charsets)

(defclass charset () ())
(defclass charset-us-ascii (charset) ())
(defclass charset-iso-8859-1 (charset) ())
(defclass charset-utf-8 (charset) ())

(defun charset-for-name (name)
  (cond ((string-equal name "sql_ascii")
	 (make-instance 'charset-us-ascii))
	((string-equal name "utf8")
	 (make-instance 'charset-utf-8))
	((string-equal name "latin1")
	 (make-instance 'charset-iso-8859-1))
	(t (error "Unknown charset: '~A'" name))))
