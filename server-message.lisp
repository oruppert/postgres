(uiop:define-package :konnekt/postgres/server-message
  (:use :common-lisp
	:konnekt/postgres/octet-stream
	:konnekt/postgres/octet-buffer
	:konnekt/postgres/char-encoding
	:konnekt/postgres/big-endian)
  (:export
   #:server-message
   #:read-server-message
   #:parse-server-message
   #:parameter-status
   #:parameter-status-name
   #:parameter-status-value
   #:ready-for-query
   #:authentication
   #:authentication-ok
   #:backend-key-data
   #:backend-key-data-process-id
   #:backend-key-data-secret-key))

(in-package :konnekt/postgres/server-message)

(defclass server-message () ()
  (:documentation "Abstract base class for all server messages."))

(defgeneric read-server-message (input)
  (:documentation "Reads a server-message from the given input."))

(defgeneric parse-server-message (message-tag octet-vector char-encoding)
  (:documentation "Parses a server-message.  The message-tag character
identifies the message.  The octet-vector contains the message-body
bytes.  The given char-encoding is used to read strings from the
octet-vector."))

(defclass parameter-status (server-message)
  ((name :initarg :name :reader parameter-status-name)
   (value :initarg :value :reader parameter-status-value))
  (:documentation "A parameter-status message.
Used by the server to inform the client about parameter values and
parameter changes.  For example client encoding and server encoding."))

(defmethod print-object ((parameter-status parameter-status) stream)
  "Prints the given parameter-status message to stream."
  (print-unreadable-object (parameter-status stream :type t)
    (format stream "~A: ~A"
	    (parameter-status-name parameter-status)
	    (parameter-status-value parameter-status))))

(defmethod parse-server-message ((message-tag (eql #\S))
				 (octet-vector vector)
				 (char-encoding char-encoding))
  "Parses a parameter-status message."
  (declare (type (vector (unsigned-byte 8)) octet-vector))
  (declare (ignore message-tag))
  (let ((buffer (make-instance 'octet-buffer :vector octet-vector)))
    (make-instance 'parameter-status
		   :name (read-string-using-encoding buffer char-encoding)
		   :value (read-string-using-encoding buffer char-encoding))))

(defclass ready-for-query (server-message) ()
  (:documentation "Send by the server when it is ready to receive requests."))

(defmethod parse-server-message ((message-tag (eql #\Z))
				 (octet-vector vector)
				 (char-encoding char-encoding))
  "Parses a ready-for-query message."
  (declare (type (vector (unsigned-byte 8) octet-vector)))
  (declare (ignore message-tag))
  (declare (ignore octet-vector))
  (declare (ignore char-encoding))
  (make-instance 'ready-for-query))

(defclass authentication (server-message) ()
  (:documentation "The abstract base class of all authentication messages."))

(defclass authentication-ok (authentication) ()
  (:documentation "Send by the server when client authentication is done."))

(defmethod parse-server-message ((message-tag (eql #\R))
				 (octet-vector vector)
				 (char-encoding char-encoding))
  "Parses an authentication message."
  (declare (type (vector (unsigned-byte 8) octet-vector)))
  (declare (ignore message-tag))
  (declare (ignore char-encoding))
  (let ((buffer (make-instance 'octet-buffer :vector octet-vector)))
    (ecase (read-signed-byte-32 buffer)
      (0 (make-instance 'authentication-ok)))))

(defclass backend-key-data (server-message)
  ((process-id :initarg :process-id :reader backend-key-data-process-id)
   (secret-key :initarg :secret-key :reader backend-key-data-secret-key))
  (:documentation "Used to issued cancel requests later."))

(defmethod print-object ((backend-key-data backend-key-data) stream)
  "Prints the given backend-key-data to stream."
  (print-unreadable-object (backend-key-data stream :type t)
    (format stream "Process-Id: ~A, Secret-Key: ~A"
	    (backend-key-data-process-id backend-key-data)
	    (backend-key-data-secret-key backend-key-data))))

(defmethod parse-server-message ((message-tag (eql #\K))
				 (octet-vector vector)
				 (char-encoding char-encoding))
  (declare (type (vector (unsigned-byte 8)) octet-vector))
  (declare (ignore message-tag))
  (declare (ignore char-encoding))
  (let ((buffer (make-instance 'octet-buffer :vector octet-vector)))
    (make-instance 'backend-key-data
		   :process-id (read-signed-byte-32 buffer)
		   :secret-key (read-signed-byte-32 buffer))))












