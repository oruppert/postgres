(uiop:define-package :konnekt/postgres/big-endian
  (:use :common-lisp :konnekt/postgres/octet-stream)
  (:export
   #:read-unsigned-byte-8
   #:read-unsigned-byte-16
   #:read-unsigned-byte-32
   #:read-unsigned-byte-64
   #:read-signed-byte-8
   #:read-signed-byte-16
   #:read-signed-byte-32
   #:read-signed-byte-64
   #:write-unsigned-byte-8
   #:write-unsigned-byte-16
   #:write-unsigned-byte-32
   #:write-unsigned-byte-64
   #:write-signed-byte-8
   #:write-signed-byte-16
   #:write-signed-byte-32
   #:write-signed-byte-64))

(in-package :konnekt/postgres/big-endian)

(defun read-unsigned-byte-8 (octet-stream)
  "Reads an unsigned-byte 8 from the given octet-stream."
  (declare (type octet-stream octet-stream))
  (read-octet octet-stream))

(defun read-unsigned-byte-16 (octet-stream)
  "Reads a big-endian unsigned-byte 16 from the given octet-stream."
  (declare (type octet-stream octet-stream))
  (let ((result 0))
    (declare (type (unsigned-byte 16) result))
    (setf result (logior (ash result 8) (read-octet octet-stream)))
    (setf result (logior (ash result 8) (read-octet octet-stream)))))

(defun read-unsigned-byte-32 (octet-stream)
  "Reads a big-endian unsigned-byte 32 from the given octet-stream."
  (declare (type octet-stream octet-stream))
  (let ((result 0))
    (declare (type (unsigned-byte 32) result))
    (setf result (logior (ash result 8) (read-octet octet-stream)))
    (setf result (logior (ash result 8) (read-octet octet-stream)))
    (setf result (logior (ash result 8) (read-octet octet-stream)))
    (setf result (logior (ash result 8) (read-octet octet-stream)))))

(defun read-unsigned-byte-64 (octet-stream)
  "Reads a big-endian unsigned-byte 64 from the given octet-stream."
  (declare (type octet-stream octet-stream))
  (let ((result 0))
    (declare (type (unsigned-byte 64) result))
    (setf result (logior (ash result 8) (read-octet octet-stream)))
    (setf result (logior (ash result 8) (read-octet octet-stream)))
    (setf result (logior (ash result 8) (read-octet octet-stream)))
    (setf result (logior (ash result 8) (read-octet octet-stream)))
    (setf result (logior (ash result 8) (read-octet octet-stream)))
    (setf result (logior (ash result 8) (read-octet octet-stream)))
    (setf result (logior (ash result 8) (read-octet octet-stream)))
    (setf result (logior (ash result 8) (read-octet octet-stream)))))

(defun read-signed-byte-8 (octet-stream)
  "Reads a signed-byte 8 from the given octet-stream."
  (declare (type octet-stream octet-stream))
  (let ((result (read-unsigned-byte-8 octet-stream)))
    (if (< result (expt 2 7))
	result
	(- result (expt 2 8)))))

(defun read-signed-byte-16 (octet-stream)
  "Reads a big-endian signed-byte 16 from the given octet-stream."
  (declare (type octet-stream octet-stream))
  (let ((result (read-unsigned-byte-16 octet-stream)))
    (if (< result (expt 2 15))
	result
	(- result (expt 2 16)))))

(defun read-signed-byte-32 (octet-stream)
  "Reads a big-endian signed-byte 32 from the given octet-stream."
  (declare (type octet-stream octet-stream))
  (let ((result (read-unsigned-byte-32 octet-stream)))
    (if (< result (expt 2 31))
	result
	(- result (expt 2 32)))))

(defun read-signed-byte-64 (octet-stream)
  "Reads a big-endian signed-byte 64 from the given octet-stream."
  (declare (type octet-stream octet-stream))
  (let ((result (read-unsigned-byte-64 octet-stream)))
    (if (< result (expt 2 63))
	result
	(- result (expt 2 64)))))

(defun write-unsigned-byte-8 (value octet-stream)
  "Writes an unsigned-byte 8 to the given octet-stream."
  (declare (type (unsigned-byte 8) value))
  (declare (type octet-stream octet-stream))
  (write-octet value octet-stream)
  (values))

(defun write-unsigned-byte-16 (value octet-stream)
  "Writes a big-endian unsigned-byte 16 to the given octet-stream."
  (declare (type (unsigned-byte 16) value))
  (declare (type octet-stream octet-stream))
  (write-octet (logand #xff (ash value -8)) octet-stream)
  (write-octet (logand #xff (ash value -0)) octet-stream)
  (values))

(defun write-unsigned-byte-32 (value octet-stream)
  "Writes a big-endian unsigned-byte 32 to the given octet-stream."
  (declare (type (unsigned-byte 32) value))
  (declare (type octet-stream octet-stream))
  (write-octet (logand #xff (ash value -24)) octet-stream)
  (write-octet (logand #xff (ash value -16)) octet-stream)
  (write-octet (logand #xff (ash value -8)) octet-stream)
  (write-octet (logand #xff (ash value -0)) octet-stream)
  (values))

(defun write-unsigned-byte-64 (value octet-stream)
  "Writes a big-endian unsigned-byte 64 to the given octet-stream."
  (declare (type (unsigned-byte 64) value))
  (declare (type octet-stream octet-stream))
  (write-octet (logand #xff (ash value -56)) octet-stream)
  (write-octet (logand #xff (ash value -48)) octet-stream)
  (write-octet (logand #xff (ash value -40)) octet-stream)
  (write-octet (logand #xff (ash value -32)) octet-stream)
  (write-octet (logand #xff (ash value -24)) octet-stream)
  (write-octet (logand #xff (ash value -16)) octet-stream)
  (write-octet (logand #xff (ash value -8)) octet-stream)
  (write-octet (logand #xff (ash value -0)) octet-stream)
  (values))

(defun write-signed-byte-8 (value octet-stream)
  "Writes a signed-byte 8 to the given octet-stream."
  (declare (type (signed-byte 8) value))
  (declare (type octet-stream octet-stream))
  (write-octet (ldb (byte 8 0) value) octet-stream)
  (values))

(defun write-signed-byte-16 (value octet-stream)
  "Writes a big-endian signed-byte 16 to the given octet-stream."
  (declare (type (signed-byte 16) value))
  (declare (type octet-stream octet-stream))
  (write-octet (ldb (byte 8 8) value) octet-stream)
  (write-octet (ldb (byte 8 0) value) octet-stream)
  (values))

(defun write-signed-byte-32 (value octet-stream)
  "Writes a big-endian signed-byte 32 to the given octet-stream."
  (declare (type (signed-byte 32) value))
  (declare (type octet-stream octet-stream))
  (write-octet (ldb (byte 8 24) value) octet-stream)
  (write-octet (ldb (byte 8 16) value) octet-stream)
  (write-octet (ldb (byte 8 8) value) octet-stream)
  (write-octet (ldb (byte 8 0) value) octet-stream)
  (values))

(defun write-signed-byte-64 (value octet-stream)
  "Writes a big-endian signed-byte 64 to the given octet-stream."
  (declare (type (signed-byte 64) value))
  (declare (type octet-stream octet-stream))
  (write-octet (ldb (byte 8 56) value) octet-stream)
  (write-octet (ldb (byte 8 48) value) octet-stream)
  (write-octet (ldb (byte 8 40) value) octet-stream)
  (write-octet (ldb (byte 8 32) value) octet-stream)
  (write-octet (ldb (byte 8 24) value) octet-stream)
  (write-octet (ldb (byte 8 16) value) octet-stream)
  (write-octet (ldb (byte 8 8) value) octet-stream)
  (write-octet (ldb (byte 8 0) value) octet-stream)
  (values))
