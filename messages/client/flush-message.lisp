(uiop:define-package :postgres/messages/client/flush-message
    (:documentation "Message that flushes the server output buffer.")
  (:use :common-lisp)
  (:use :postgres/messages/client-message)
  (:export :flush-message))

(in-package :postgres/messages/client/flush-message)

(defclass flush-message (client-message) ()
  (:default-initargs :tag #\H)
  (:documentation "Flushes the server output buffer."))


