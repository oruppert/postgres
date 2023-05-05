(uiop:define-package :postgres/handlers/message-handler
    (:documentation "")
  (:use :common-lisp)
  (:export :message-handler)
  (:export :handle-message))

(in-package :postgres/handlers/message-handler)

(defclass message-handler () ()
  (:documentation "The message-handler protocol class."))

(defgeneric handle-message (handler message)
  (:documentation "Handles the given message."))








