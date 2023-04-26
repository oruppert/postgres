(uiop:define-package :postgres/handlers/message-handler
    (:documentation "")
  (:use :common-lisp)
  (:export :message-handler)
  (:export :message-handler-run)
  (:export :handle-message))

(in-package :postgres/handlers/message-handler)

(defclass message-handler () ()
  (:documentation "The message-handler protocol class."))

(defgeneric message-handler-run (database handler)
  (:documentation "Run message processing loop.
Methods of this function may or may not return."))

(defgeneric handle-message (database handler message)
  (:documentation "Handles the given message."))








