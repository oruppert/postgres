(uiop:define-package :konnekt/postgres/char-encoding
    (:use :common-lisp :konnekt/postgres/octet-stream)
  (:export
   #:char-encoding
   #:char-encoding-us-ascii
   #:char-encoding-iso-8859-1
   #:char-encoding-utf-8
   #:read-char-using-encoding
   #:write-char-using-encoding
   #:read-string-using-encoding
   #:write-string-using-encoding))

(in-package :konnekt/postgres/char-encoding)

(defclass char-encoding () ()
  (:documentation "Abstract base class of all character encodings."))

(defmethod print-object ((char-encoding char-encoding) stream)
  "Print the given char-encoding to stream."
  (format stream "#<~A>" (type-of char-encoding)))

(defgeneric read-char-using-encoding
    (octet-stream char-encoding &optional eof-error-p eof-value)
  (:documentation
   "Reads a character from octet-stream using the given char-encoding."))

(defgeneric write-char-using-encoding (character octet-stream char-encoding)
  (:documentation
   "Writes character to octet-stream using the given char-encoding."))

(defun read-string-using-encoding (octet-stream char-encoding)
  "Reads a string from octet-stream using the given char-encoding."
  (declare (type octet-stream octet-stream))
  (declare (type char-encoding char-encoding))
  (loop with result = (make-array 0
				  :element-type 'character
				  :adjustable t
				  :fill-pointer 0)
	with null-char = (code-char 0)
	for char = (read-char-using-encoding
		    octet-stream char-encoding nil null-char)
	until (char= char null-char)
	do (vector-push-extend char result)
	finally (return result)))

(defun write-string-using-encoding
    (string octet-stream char-encoding &key (null-terminated t))
  "Writes string to octet-stream using the given char-encoding.
Writes a final zero octet if null-terminated is true."
  (declare (type string string))
  (declare (type octet-stream octet-stream))
  (declare (type char-encoding char-encoding))
  (loop for char across string
	do (write-char-using-encoding char octet-stream char-encoding))
  (when null-terminated
    (write-octet 0 octet-stream))
  (values))

;;; US-ASCII
(defclass char-encoding-us-ascii (char-encoding) ()
  (:documentation "The us-ascii character encoding."))

(defmethod read-char-using-encoding ((octet-stream octet-stream)
				     (char-encoding char-encoding-us-ascii)
				     &optional eof-error-p eof-value)
  "Reads an us-ascii character from octet-stream."
  (declare (ignore char-encoding))
  (let ((octet (read-octet octet-stream nil nil)))
    (if (null octet)
	(if (null eof-error-p)
	    eof-value
	    (error "end of stream"))
	(if (< octet #o200)
	    (code-char octet)
	    (error "octet ~A is not in the us-ascii range" octet)))))

(defmethod write-char-using-encoding ((character character)
				      (octet-stream octet-stream)
				      (char-encoding char-encoding-us-ascii))
  "Writes an us-ascii character to octet-stream."
  (declare (ignore char-encoding))
  (let ((char-code (char-code character)))
    (if (< char-code #o200)
	(write-octet char-code octet-stream)
	(error "character ~S is not representable in us-ascii." character)))
  (values))

;;; ISO-8859-1
(defclass char-encoding-iso-8859-1 (char-encoding) ()
  (:documentation "The iso-8859-1 character encoding."))

(defmethod read-char-using-encoding ((octet-stream octet-stream)
				     (char-encoding char-encoding-iso-8859-1)
				     &optional eof-error-p eof-value)
  "Reads an iso-8859-1 character from octet-stream."
  (declare (ignore char-encoding))
  (let ((octet (read-octet octet-stream nil nil)))
    (if (null octet)
	(if (null eof-error-p)
	    eof-value
	    (error "end of stream"))
	(code-char octet))))

(defmethod write-char-using-encoding ((character character)
				      (octet-stream octet-stream)
				      (char-encoding char-encoding-iso-8859-1))
  "Writes an iso-8859-1 character to octet-stream."
  (declare (ignore char-encoding))
  (let ((char-code (char-code character)))
    (if (< char-code #o400)
	(write-octet char-code octet-stream)
	(error "character ~S is not representable in iso-8859-1." character)))
  (values))

;;; UTF-8
(defclass char-encoding-utf-8 (char-encoding) ()
  (:documentation "The utf-8 character encoding."))

(defmethod read-char-using-encoding ((octet-stream octet-stream)
				     (char-encoding char-encoding-utf-8)
				     &optional eof-error-p eof-value)
  "Reads an utf-8 character from octet-stream."
  (declare (ignore char-encoding))
  (let ((octet (read-octet octet-stream nil nil)))
    (if (null octet)
	(if (null eof-error-p)
	    eof-value
	    (error "end of stream"))
	(flet ((next () (read-octet octet-stream t)))
	  (code-char
	   (cond ((= #b0 (ash octet -7))
		  (logand #b1111111 octet))
		 ((= #b110 (ash octet -5))
		  (let ((r (logand #b11111 octet)))
		    (setf r (logior (ash r 6) (logand #b111111 (next))))))
		 ((= #b1110 (ash octet -4))
		  (let ((r (logand #b1111 octet)))
		    (setf r (logior (ash r 6) (logand #b111111 (next))))
		    (setf r (logior (ash r 6) (logand #b111111 (next))))))
		 ((= #b11110 (ash octet -3))
		  (let ((r (logand #b111 octet)))
		    (setf r (logior (ash r 6) (logand #b111111 (next))))
		    (setf r (logior (ash r 6) (logand #b111111 (next))))
		    (setf r (logior (ash r 6) (logand #b111111 (next))))))
		 (t (error "invalid utf-8 starting octet: ~A" octet))))))))

(defmethod write-char-using-encoding ((character character)
				      (octet-stream octet-stream)
				      (char-encoding char-encoding-utf-8))
  "Writes an utf-8 character to octet-stream."
  (declare (ignore char-encoding))
  ;; https://en.wikipedia.org/wiki/UTF-8 Octal
  (let ((code (char-code character)))
    (cond ((< code #o200)
	   (write-octet code octet-stream))
	  ((< code #o4000)
	   (write-octet (logior #o300 (ldb (byte 6 6) code)) octet-stream)
	   (write-octet (logior #o200 (ldb (byte 6 0) code)) octet-stream))
	  ((< code #o100000)
	   (write-octet (logior #o340 (ldb (byte 3 12) code)) octet-stream)
	   (write-octet (logior #o200 (ldb (byte 6 6) code)) octet-stream)
	   (write-octet (logior #o200 (ldb (byte 6 0) code)) octet-stream))
	  ((< code #o200000)
	   (write-octet (logior #o350 (ldb (byte 3 12) code)) octet-stream)
	   (write-octet (logior #o200 (ldb (byte 6 6) code)) octet-stream)
	   (write-octet (logior #o200 (ldb (byte 6 0) code)) octet-stream))
	  ((< code #o4200000)
	   (write-octet (logior #o360 (ldb (byte 3 18) code)) octet-stream)
	   (write-octet (logior #o200 (ldb (byte 6 12) code)) octet-stream)
	   (write-octet (logior #o200 (ldb (byte 6 6) code)) octet-stream)
	   (write-octet (logior #o200 (ldb (byte 6 0) code)) octet-stream))))
  (values))




