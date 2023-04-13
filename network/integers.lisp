(uiop:define-package :konnekt/postgres/network/integers
  (:use :common-lisp :konnekt/postgres/network/generics)
  (:export
   ;; read unsigned big endian
   #:read-uint16
   #:read-uint32
   #:read-uint64
   ;; read signed big endian
   #:read-int8
   #:read-int16
   #:read-int32
   #:read-int64
   ;; write unsigned big endian
   #:write-uint16
   #:write-uint32
   #:write-uint64
   ;; write signed big endian
   #:write-int8
   #:write-int16
   #:write-int32
   #:write-int64))

(in-package :konnekt/postgres/network/integers)

;;; Reading
(defun read-uint16 (input)
  (let ((value 0))
    (declare (type (unsigned-byte 16) value))
    (setf (ldb (byte 8 8) value) (read-uint8 input))
    (setf (ldb (byte 8 0) value) (read-uint8 input))
    (values value)))

(defun read-uint32 (input)
  (let ((value 0))
    (declare (type (unsigned-byte 32) value))
    (setf (ldb (byte 8 24) value) (read-uint8 input))
    (setf (ldb (byte 8 16) value) (read-uint8 input))
    (setf (ldb (byte 8 8) value) (read-uint8 input))
    (setf (ldb (byte 8 0) value) (read-uint8 input))
    (values value)))

(defun read-uint64 (input)
  (let ((value 0))
    (declare (type (unsigned-byte 64) value))
    (setf (ldb (byte 8 56) value) (read-uint8 input))
    (setf (ldb (byte 8 48) value) (read-uint8 input))
    (setf (ldb (byte 8 40) value) (read-uint8 input))
    (setf (ldb (byte 8 32) value) (read-uint8 input))
    (setf (ldb (byte 8 24) value) (read-uint8 input))
    (setf (ldb (byte 8 16) value) (read-uint8 input))
    (setf (ldb (byte 8 8) value) (read-uint8 input))
    (setf (ldb (byte 8 0) value) (read-uint8 input))
    (values value)))

(defun read-int8 (input)
  (let ((value (read-uint8 input)))
    (if (> value #x7f)
	(- value #x100)
	value)))

(defun read-int16 (input)
  (let ((value (read-uint16 input)))
    (if (> value #x7fff)
	(- value #x10000)
	value)))

(defun read-int32 (input)
  (let ((value (read-uint32 input)))
    (if (> value #x7fffffff)
	(- value #x100000000)
	value)))

(defun read-int64 (input)
  (let ((value (read-uint64 input)))
    (if (> value #x7fffffffffffffff)
	(- value #x10000000000000000)
	value)))

;;; Writing
(defun write-uint16 (value output)
  (declare (type (unsigned-byte 16) value))
  (write-uint8 (ldb (byte 8 8) value) output)
  (write-uint8 (ldb (byte 8 0) value) output)
  (values))

(defun write-uint32 (value output)
  (declare (type (unsigned-byte 32) value))
  (write-uint8 (ldb (byte 8 24) value) output)
  (write-uint8 (ldb (byte 8 16) value) output)
  (write-uint8 (ldb (byte 8 8) value) output)
  (write-uint8 (ldb (byte 8 0) value) output)
  (values))

(defun write-uint64 (value output)
  (declare (type (unsigned-byte 64) value))
  (write-uint8 (ldb (byte 8 56) value) output)
  (write-uint8 (ldb (byte 8 48) value) output)
  (write-uint8 (ldb (byte 8 40) value) output)
  (write-uint8 (ldb (byte 8 32) value) output)
  (write-uint8 (ldb (byte 8 24) value) output)
  (write-uint8 (ldb (byte 8 16) value) output)
  (write-uint8 (ldb (byte 8 8) value) output)
  (write-uint8 (ldb (byte 8 0) value) output)
  (values))

(defun write-int8 (value output)
  (declare (type (signed-byte 8) value))
  (write-uint8 value output)
  (values))

(defun write-int16 (value output)
  (declare (type (signed-byte 16) value))
  (write-uint8 (ldb (byte 8 8) value) output)
  (write-uint8 (ldb (byte 8 0) value) output)
  (values))

(defun write-int32 (value output)
  (declare (type (signed-byte 32) value))
  (write-uint8 (ldb (byte 8 24) value) output)
  (write-uint8 (ldb (byte 8 16) value) output)
  (write-uint8 (ldb (byte 8 8) value) output)
  (write-uint8 (ldb (byte 8 0) value) output)
  (values))

(defun write-int64 (value output)
  (declare (type (signed-byte 64) value))
  (write-uint8 (ldb (byte 8 56) value) output)
  (write-uint8 (ldb (byte 8 48) value) output)
  (write-uint8 (ldb (byte 8 40) value) output)
  (write-uint8 (ldb (byte 8 32) value) output)
  (write-uint8 (ldb (byte 8 24) value) output)
  (write-uint8 (ldb (byte 8 16) value) output)
  (write-uint8 (ldb (byte 8 8) value) output)
  (write-uint8 (ldb (byte 8 0) value) output)
  (values))
