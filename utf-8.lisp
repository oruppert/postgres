(uiop:define-package :postgres/utf-8
    (:documentation "Functions for reading and writing utf-8.")
  (:use
   :common-lisp
   :postgres/octet-stream)
  (:export
   :read-utf-8-char
   :read-utf-8-string
   :write-utf-8-char
   :write-utf-8-string))

(in-package :postgres/utf-8)

(defun read-utf-8-char (octet-stream &optional (eof-error-p t) eof-value)
  "Reads an utf-8 character from the given octet-stream."
  (let ((octet (read-octet octet-stream nil nil)))
    (if (null octet)
	(if (null eof-error-p)
	    eof-value
	    (error "end of stream"))
	(flet ((next ()
		 (let ((octet (read-octet octet-stream)))
		   ;; TODO: check invalid continuation byte
		   (logand #b111111 octet))))
	  (code-char
	   (cond ((= #b0 (ash octet -7))
		  (logand #b1111111 octet))
		 ((= #b110 (ash octet -5))
		  (let ((r (logand #b11111 octet)))
		    (setf r (logior (ash r 6) (next)))))
		 ((= #b1110 (ash octet -4))
		  (let ((r (logand #b1111 octet)))
		    (setf r (logior (ash r 6) (next)))
		    (setf r (logior (ash r 6) (next)))))
		 ((= #b11110 (ash octet -3))
		  (let ((r (logand #b111 octet)))
		    (setf r (logior (ash r 6) (next)))
		    (setf r (logior (ash r 6) (next)))
		    (setf r (logior (ash r 6) (next)))))
		 (t (error "invalid utf-8 starting octet: ~A" octet))))))))

(defun read-utf-8-string (octet-stream)
  "Reads an utf-8 string from the given octet-stream.
Reading stops at a null character or at end of stream."
  (loop with result = (make-array 0
				  :element-type 'character
				  :adjustable t
				  :fill-pointer 0)
	with null-char = (code-char 0)
	for char = (read-utf-8-char octet-stream nil null-char)
	until (char= char null-char)
	do (vector-push-extend char result)
	finally (return result)))

(defun write-utf-8-char (char octet-stream)
  "Writes an utf-8 character to the given octet-stream."
  (let ((code (char-code char)))
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

(defun write-utf-8-string (string octet-stream &optional (null-terminated t))
  "Writes an utf-8 string to the given octet-stream.
Writes a final zero octet if null-terminated is true."
  (loop for char across string do
    (write-utf-8-char char octet-stream))
  (when null-terminated
    (write-octet 0 octet-stream))
  (values))
