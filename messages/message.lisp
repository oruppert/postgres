(uiop:define-package :postgres/messages/message
    (:documentation "Interface for sending and receiving messages.")
  (:use :common-lisp)
  (:export :message)
  (:export :receive-message)
  (:export :send-message)
  (:export :read-message)
  (:export :write-message))

(in-package :postgres/messages/message)

(defclass message () ()
  (:documentation "Message interface class."))

(defgeneric send-message (message output)
  (:documentation "Sends message to the given output.
--Methods of this generic function may be non-blocking.
Methods of this generic function are non-blocking.
Returns no values."))

(defgeneric receive-message (input)
  (:documentation "Receives a message from the given input.
Methods of this generic function are blocking.
Returns the received message."))

(defgeneric read-message (octet-stream)
  (:documentation "Reads a message from the given octet-stream.
Methods of this generic function are blocking.
Returns a fresh message."))

(defgeneric write-message (message octet-stream)
  (:documentation "Writes message to the given octet-stream.
Methods of this generic function are blocking.
Returns no values."))











