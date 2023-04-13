(uiop:define-package :konnekt/postgres/buffer
    (:use :common-lisp)
  (:export
   #:buffer
   #:make-buffer
   #:buffer-vector
   #:buffer-position
   #:read-octet
   #:write-octet
   #:read-signed-byte-16
   #:read-signed-byte-32
   #:write-signed-byte-16
   #:write-signed-byte-32
   #:charset
   #:charset-us-ascii
   #:charset-iso-8859-1
   #:charset-utf-8
   #:write-string-using-charset
   #:read-string-using-charset))

(in-package :konnekt/postgres/buffer)

(defstruct (buffer (:constructor make-buffer (&optional vector position)))
  (vector (make-array 0
		      :element-type '(unsigned-byte 8)
		      :adjustable t
		      :fill-pointer 0)
   :type (vector (unsigned-byte 8) *))
  (position 0 :type (integer 0)))

(defun read-octet (buffer)
  "Reads an unsigned-byte 8 from the given buffer."
  (declare (type buffer buffer))
  (prog1 (aref (buffer-vector buffer)
	       (buffer-position buffer))
      (incf (buffer-position buffer))))

(defun write-octet (octet buffer)
  "Writes an unsigned-byte 8 to the given buffer."
  (declare (type (unsigned-byte 8) octet))
  (declare (type buffer buffer))
  (if (< (buffer-position buffer)
	 (length (buffer-vector buffer)))
      (setf (aref (buffer-vector buffer)
		  (buffer-position buffer))
	    octet)
      (vector-push-extend octet (buffer-vector buffer)))
  (incf (buffer-position buffer))
  (values))

(defun read-signed-byte-16 (buffer)
  "Reads a big endian signed-byte 16 from the given buffer."
  (declare (type buffer buffer))
  (let ((result 0))
    (declare (type (unsigned-byte 16) result))
    (setf (ldb (byte 8 24) result) (read-octet buffer))
    (setf (ldb (byte 8 16) result) (read-octet buffer))
    (setf (ldb (byte 8 8) result) (read-octet buffer))
    (setf (ldb (byte 8 0) result) (read-octet buffer))
    (if (< result #x8000)
	result
	(- result #x10000))))

(defun read-signed-byte-32 (buffer)
  "Reads a big endian signed-byte 32 from the given buffer."
  (declare (type buffer buffer))
  (let ((result 0))
    (declare (type (unsigned-byte 32) result))
    (setf (ldb (byte 8 24) result) (read-octet buffer))
    (setf (ldb (byte 8 16) result) (read-octet buffer))
    (setf (ldb (byte 8 8) result) (read-octet buffer))
    (setf (ldb (byte 8 0) result) (read-octet buffer))
    (if (< result #x80000000)
	result
	(- result #x100000000))))

(defun write-signed-byte-16 (value buffer)
  "Writes a big endian signed-byte 16 from the given buffer."
  (declare (type (signed-byte 16) value))
  (declare (type buffer buffer))
  (write-octet (ldb (byte 8 8) value) buffer)
  (write-octet (ldb (byte 8 0) value) buffer)
  (values))

(defun write-signed-byte-32 (value buffer)
  "Writes a big endian signed-byte 32 from the given buffer."
  (declare (type (signed-byte 32) value))
  (declare (type buffer buffer))
  (write-octet (ldb (byte 8 24) value) buffer)
  (write-octet (ldb (byte 8 16) value) buffer)
  (write-octet (ldb (byte 8 8) value) buffer)
  (write-octet (ldb (byte 8 0) value) buffer)
  (values))

(defclass charset () ())
(defclass charset-us-ascii (charset) ())
(defclass charset-iso-8859-1 (charset) ())
(defclass charset-utf-8 (charset) ())

(defgeneric write-char-using-charset (char buffer charset)
  (:documentation "Writes character to buffer using the given charset."))

(defmethod write-char-using-charset (char buffer (charset charset-us-ascii))
  "Writes character to buffer using the us-ascii encoding."
  (declare (type character char))
  (declare (type buffer buffer))
  (let ((code (char-code char)))
    (if (< code #o200)
	(write-octet code buffer)
	(error "The given character ~S is not in the us-ascii range." char))))

(defmethod write-char-using-charset (char buffer (charset charset-iso-8859-1))
  "Writes character to buffer using the iso-8859-1 encoding."
  (declare (type character char))
  (declare (type buffer buffer))
  (let ((code (char-code char)))
    (if (< code #o400)
	(write-octet code buffer)
	(error "The given character ~S is not in the iso-8859-1 range." char))))

#+nil
(defmethod write-char-using-encoding ((character character)
				      (stream octet-stream)
				      (encoding character-encoding-iso-8859-1))
  (declare (ignore encoding))
  (let ((char-code (char-code character)))
    (if (< char-code #o400)
	(write-octet char-code buffer)
	(error "Character ~S is not in the iso-8859-1 range." character))))




(defmethod write-char-using-charset (char buffer (charset charset-utf-8))
  "Writes character to buffer using the utf-8 encoding."
  (declare (type character char))
  (declare (type buffer buffer))
  ;; https://en.wikipedia.org/wiki/UTF-8 Octal
  (let ((code (char-code char)))
    (cond ((< code #o200)
	   (write-octet code buffer))
	  ((< code #o4000)
	   (write-octet (logior #o300 (ldb (byte 6 6) code)) buffer)
	   (write-octet (logior #o200 (ldb (byte 6 0) code)) buffer))
	  ((< code #o100000)
	   (write-octet (logior #o340 (ldb (byte 3 12) code)) buffer)
	   (write-octet (logior #o200 (ldb (byte 6 6) code)) buffer)
	   (write-octet (logior #o200 (ldb (byte 6 0) code)) buffer))
	  ((< code #o200000)
	   (write-octet (logior #o350 (ldb (byte 3 12) code)) buffer)
	   (write-octet (logior #o200 (ldb (byte 6 6) code)) buffer)
	   (write-octet (logior #o200 (ldb (byte 6 0) code)) buffer))
	  ((< code #o4200000)
	   (write-octet (logior #o360 (ldb (byte 3 18) code)) buffer)
	   (write-octet (logior #o200 (ldb (byte 6 12) code)) buffer)
	   (write-octet (logior #o200 (ldb (byte 6 6) code)) buffer)
	   (write-octet (logior #o200 (ldb (byte 6 0) code)) buffer)))))

(defun write-string-using-charset (string buffer charset &key (final-null t))
  "Writes string to buffer using the given charset.
If final-null is true (the default), then write a final zero octet."
  (declare (type string string))
  (declare (type buffer buffer))
  (declare (type charset charset))
  (loop for char across string
	do (write-char-using-charset char buffer charset))
  (when final-null (write-octet 0 buffer))
  (values))




;(defgeneric write-octet (octet port))
;;(defgeneric write-octet-vector (vector port))









(defgeneric read-char-using-charset (buffer charset)
  (:documentation "Reads a character from buffer using the given charset."))

(defmethod read-char-using-charset (buffer (charset charset-us-ascii))
  "Reads a character from buffer using the us-ascii charset."
  (declare (type buffer buffer))
  (declare (ignore charset))
  (let ((octet (read-octet buffer)))
    (if (< octet #o200)
	(code-char octet)
	(error "Octet ~A is not in the us-ascii range." octet))))

(defmethod read-char-using-charset (buffer (charset charset-iso-8859-1))
  "Reads a character from buffer using the is-8859-1 charset."
  (declare (type buffer buffer))
  (declare (ignore charset))
  (let ((octet (read-octet buffer)))
    (code-char octet)))

(defmethod read-char-using-charset (buffer (charset charset-utf-8))
  "Reads a character from buffer using the utf-8 charset."
  (declare (type buffer buffer))
  (declare (ignore charset))
  (flet ((read-continuation-octet ()
	   (let ((octet (read-octet buffer)))
	     (if (= #b10 (ash octet -6))
		 (ldb (byte 6 0) octet)
		 (error "Invalid utf-8 continuation octet: ~b" octet)))))
    (let ((octet (read-octet buffer)))
      (code-char
       (cond
	 ((= #b0000 (ash octet -7))
	  octet)
	 ((= #b0110 (ash octet -5))
	  (let ((result (ldb (byte 5 0) octet)))
	    (setf result (logior (ash result 6) (read-continuation-octet)))))
	 ((= #b1110 (ash octet -4))
	  (let ((result (ldb (byte 4 0) octet)))
	    (setf result (logior (ash result 6) (read-continuation-octet)))
	    (setf result (logior (ash result 6) (read-continuation-octet)))))
	 ((= #b11110 (ash octet -3))
	  (let ((result (ldb (byte 3 0) octet)))
	    (setf result (logior (ash result 6) (read-continuation-octet)))
	    (setf result (logior (ash result 6) (read-continuation-octet)))
	    (setf result (logior (ash result 6) (read-continuation-octet)))))
	 (t (error "Invalid utf-8 starting octet: ~A" octet)))))))

(defun read-string-using-charset (buffer charset)
  "Reads a string from buffer using the given charset.
Reading ends if either a null character is read or the end of buffer
is reached.  Note that the trailing null character is not part of the
returned string."
  (loop with result = (make-array 0
				  :element-type 'character
				  :adjustable t
				  :fill-pointer 0)
	until (= (buffer-position buffer)
		 (length (buffer-vector buffer)))
	for char = (read-char-using-charset buffer charset)
	when (char= char (code-char 0)) do (return result)
	else do (vector-push-extend char result)
	finally (return result)))








