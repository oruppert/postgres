(uiop:define-package :postgres/io/character-encoding
    (:use :common-lisp :postgres/io/octet-stream)
  (:export
   #:character-encoding
   #:character-encoding-us-ascii
   #:character-encoding-iso-8859-1
   #:character-encoding-utf-8
   #:read-character-using-encoding
   #:read-string-using-encoding
   #:write-character-using-encoding
   #:write-string-using-encoding))

(in-package :postgres/io/character-encoding)

(defclass character-encoding () ()
  (:documentation "Abstract base class of all character encodings."))

(defmethod print-object ((character-encoding character-encoding) stream)
  "Print the given character encoding to stream."
  (format stream "#<~A>" (type-of character-encoding)))

(defgeneric read-character-using-encoding
    (octet-stream character-encoding &optional eof-error-p eof-value)
  (:documentation
   "Reads a character from octet-stream using the given character encoding."))

(defun read-string-using-encoding (octet-stream character-encoding)
  "Reads a string from octet-stream using the given character encoding."
  (declare (type octet-stream octet-stream))
  (declare (type character-encoding character-encoding))
  (loop with result = (make-array 0
				  :element-type 'character
				  :adjustable t
				  :fill-pointer 0)
	with null-char = (code-char 0)
	for char = (read-char-using-encoding octet-stream
					     character-encoding
					     nil
					     null-char)
	until (char= char null-char)
	do (vector-push-extend char result)
	finally (return result)))

(defgeneric write-character-using-encoding (character octet-stream encoding)
  (:documentation "Writes character to octet-stream using the given encoding."))

(defun write-string-using-encoding
    (string octet-stream character-encoding &key (null-terminated t))
  "Writes string to octet-stream using the given character encoding.
Writes a final zero octet if null-terminated is true."
  (declare (type string string))
  (declare (type octet-stream octet-stream))
  (declare (type character-encoding character-encoding))
  (loop for character across string
	do (write-character-using-encoding character
					   octet-stream
					   character-encoding))
  (when null-terminated
    (write-octet 0 octet-stream))
  (values))

;;; US-ASCII
(defclass character-encoding-us-ascii (character-encoding) ()
  (:documentation "The us-ascii character encoding."))

(defmethod read-character-using-encoding
    ((octet-stream octet-stream)
     (encoding character-encoding-us-ascii)
     &optional eof-error-p eof-value)
  "Reads an us-ascii encoded character from octet-stream."
  (declare (ignore encoding))
  (let ((octet (read-octet octet-stream nil nil)))
    (if (null octet)
	(if (null eof-error-p)
	    eof-value
	    (error "end of stream"))
	(if (< octet #o200)
	    (code-char octet)
	    (error "octet ~A is not in the us-ascii range" octet)))))

(defmethod write-character-using-encoding
    ((character character)
     (octet-stream octet-stream)
     (character-encoding character-encoding-us-ascii))
  "Writes an us-ascii character to octet-stream."
  (declare (ignore character-encoding))
  (let ((char-code (char-code character)))
    (if (< char-code #o200)
	(write-octet char-code octet-stream)
	(error "character ~S is not representable in us-ascii." character)))
  (values))

;;; ISO-8859-1
(defclass character-encoding-iso-8859-1 (character-encoding) ()
  (:documentation "The iso-8859-1 character encoding."))

(defmethod read-character-using-encoding
    ((octet-stream octet-stream)
     (character-encoding character-encoding-iso-8859-1)
     &optional eof-error-p eof-value)
  "Reads an iso-8859-1 character from octet-stream."
  (declare (ignore character-encoding))
  (let ((octet (read-octet octet-stream nil nil)))
    (if (null octet)
	(if (null eof-error-p)
	    eof-value
	    (error "end of stream"))
	(code-char octet))))

(defmethod write-character-using-encoding
    ((character character)
     (octet-stream octet-stream)
     (character-encoding char-encoding-iso-8859-1))
  "Writes an iso-8859-1 encoded character to octet-stream."
  (declare (ignore character-encoding))
  (let ((char-code (char-code character)))
    (if (< char-code #o400)
	(write-octet char-code octet-stream)
	(error "character ~S is not representable in iso-8859-1." character)))
  (values))

;;; UTF-8
(defclass character-encoding-utf-8 (character-encoding-utf-8-encoding) ()
  (:documentation "The utf-8 character encoding."))

(defmethod read-character-using-encoding
    ((octet-stream octet-stream)
     (character-encoding character-encoding-utf-8)
     &optional eof-error-p eof-value)
  "Reads an utf-8 character from octet-stream."
  (declare (ignore character-encoding))
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

(defmethod write-character-using-encoding
    ((character character)
     (octet-stream octet-stream)
     (character-encoding character-encoding-utf-8))
  "Writes an utf-8 character to octet-stream."
  (declare (ignore character-encoding))
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




