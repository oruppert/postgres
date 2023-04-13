(uiop:define-package :konnekt/postgres/handlers/simple-query-handler
  (:use :common-lisp
	:konnekt/postgres/types
	:konnekt/postgres/io/connection
	:konnekt/postgres/handlers/message-handler
	:konnekt/postgres/messages/row-description-message
	:konnekt/postgres/messages/command-complete-message
	:konnekt/postgres/messages/ready-for-query-message
	:konnekt/postgres/messages/data-row-message
)
  (:export :simple-query-handler))

(in-package :konnekt/postgres/handlers/simple-query-handler)

(defclass simple-query-handler (message-handler)
  (fields (rows :initform nil)))

(defmethod handle-message (database (handler simple-query-handler)
				    (message row-description-message))
  (with-slots (fields) handler
    (setf fields (row-description-fields message))))

(defmethod handle-message (database (handler simple-query-handler)
				    (message command-complete-message)))

(defmethod handle-message ((connection connection)
			   (handler simple-query-handler)
			   (message data-row-message))
  (let ((charset (connection-charset connection)))
    (with-slots (fields rows) handler
      (loop for field in fields
	    for type = (postgres-type (field-description-type-id field))
	    for bytes in (data-row-columns message)
	    collect (parse-field-value
		     (field-format-convert bytes charset
					   (field-format field))
		     type)
	      into row
	    finally (push row rows)))))

(defmethod handle-message (database (handler simple-query-handler)
				    (message ready-for-query-message))
  (with-slots (rows) handler
    (abort-message-loop (nreverse rows))))

(defgeneric parse-field-value (bytes type))

(defmethod parse-field-value ((null null)
			      (type postgres-type))
  nil)

(defmethod parse-field-value ((bytes vector)
			      (type postgres-type))
  (cons bytes type))

(defmethod parse-field-value ((string string)
			      (type postgres-string-type))
  string)

(defmethod parse-field-value ((string string)
			      (type postgres-integer-type))
  (parse-integer string))

(defmethod parse-field-value ((bytes vector)
			      (type postgres-string-type))
  ;; todo charset
  (flex:octets-to-string bytes :external-format :utf8))


