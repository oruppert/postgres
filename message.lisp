(uiop:define-package :postgres/message
    (:use :common-lisp)
  (:export :message
	   :read-message
	   :send-message)
  (:documentation "Interface for reading and sending messages."))

(in-package :postgres/message)

(defclass message () ()
  (:documentation "Message base class."))

(defgeneric read-message (input)
  (:documentation "Reads a message from the given input.
Methods of this generic function are always blocking.
Returns a fresh message."))

(defgeneric send-message (message output)
  (:documentation "Sends message to the given output.
Methods of this generic function may be non-blocking.
Returns no values."))








