(uiop:define-package :postgres/messages/client-message
    (:documentation "Messages send to the database server.")
  (:use :common-lisp)
  (:use :postgres/messages/message)
  (:use :postgres/streams/big-endian)
  (:use :postgres/streams/octet-buffer)
  (:use :postgres/streams/octet-stream)
  (:export :client-message)
  (:export :write-message-body))

(in-package :postgres/messages/client-message)

(defclass client-message (message)
  ((tag
    :documentation "The first byte of the message or null."
    :type (or character null)
    :initarg :tag
    :initform (error "No tag given.")))
  (:documentation "The client-message base class."))

(defgeneric write-message-body (message stream)
  (:documentation "Writes the message body to the given stream."))

(defmethod write-message-body ((message client-message)
			       (stream octet-stream))
  "Default implementation that does nothing."
  (declare (ignore message))
  (declare (ignore stream))
  (values))

(defmethod write-message ((message client-message)
			  (stream octet-stream))
  (let ((buffer (make-instance 'octet-buffer)))
    (write-message message buffer)
    (write-octet-vector (buffer-vector buffer) stream))
  (values))

(defmethod write-message ((message client-message)
			  (buffer octet-buffer))
  ;; Maybe write message tag.
  (with-slots (tag) message
    (unless (null tag)
      (let ((octet (char-code tag)))
	(write-octet octet buffer))))
  (let ((start (buffer-position buffer)))
    ;; write dummy message length to preserve space
    (write-signed-byte-32 0 buffer)
    ;; write message body
    (write-message-body message buffer)
    ;; write actual message length
    (let* ((end (buffer-position buffer))
	   (actual-length (- end start)))
      (setf (buffer-position buffer) start)
      (write-signed-byte-32 actual-length buffer)
      (setf (buffer-position buffer) end)))
  (values))
