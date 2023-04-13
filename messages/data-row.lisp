(uiop:define-package :konnekt/postgres/messages/data-row
  (:use :common-lisp
	:konnekt/postgres/messages/message
	:konnekt/postgres/network/charsets
	:konnekt/postgres/network/integers)
  (:export
   #:data-row))

(in-package :konnekt/postgres/messages/data-row)

(defclass data-row (message)
  ((data :initarg :data :reader data-row-data)))

(defmethod read-message-body ((message-tag (eql #\D))
			      (input vector)
			      (charset charset))
  (declare (type (vector (unsigned-byte 8) *) input))
  (make-instance 'data-row-message :data input))


