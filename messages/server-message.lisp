(uiop:define-package :postgres/messages/server-message
  (:use :common-lisp))

(in-package :postgres/messages/server-message)

(defclass server-message (message) ()
  (:documentation "Abstract base class for all server messages."))

#+nil
(defclass ready-for-query (server-message) ()
  (:documentation "Send by the server when it is ready to receive requests."))

#+nil
(defmethod parse-server-message ((message-tag (eql #\Z))
				 (octet-vector vector)
				 (char-encoding char-encoding))
  "Parses a ready-for-query message."
  (declare (type (vector (unsigned-byte 8) octet-vector)))
  (declare (ignore message-tag))
  (declare (ignore octet-vector))
  (declare (ignore char-encoding))
  (make-instance 'ready-for-query))

#+nil
(defclass authentication (server-message) ()
  (:documentation "The abstract base class of all authentication messages."))

#+nil
(defclass authentication-ok (authentication) ()
  (:documentation "Send by the server when client authentication is done."))

#+nil
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

#+nil
(defclass backend-key-data (server-message)
  ((process-id :initarg :process-id :reader backend-key-data-process-id)
   (secret-key :initarg :secret-key :reader backend-key-data-secret-key))
  (:documentation "Used to issued cancel requests later."))

#+nil
(defmethod print-object ((backend-key-data backend-key-data) stream)
  "Prints the given backend-key-data to stream."
  (print-unreadable-object (backend-key-data stream :type t)
    (format stream "Process-Id: ~A, Secret-Key: ~A"
	    (backend-key-data-process-id backend-key-data)
	    (backend-key-data-secret-key backend-key-data))))

#+nil
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












