(uiop:define-package :konnekt/postgres/messages/row-description-message
  (:use :common-lisp
	:konnekt/postgres/messages/message
	:konnekt/postgres/io/integers
	:konnekt/postgres/io/strings
	:konnekt/postgres/io/charsets)
  (:export
   #:field-description
   #:field-description-name
   #:field-description-table-oid
   #:field-description-attribute-number
   #:field-description-type-id
   #:field-description-data-type-size
   #:field-description-type-modifier
   #:field-description-format-code
   #:row-description-message
   #:row-description-fields
   #:field-format
   #:field-text-format
   #:field-binary-format
   #:field-format-convert))

(in-package :konnekt/postgres/messages/row-description-message)

(defclass field-description ()
  ((name
    :initarg :name
    :reader field-description-name)
   (table-oid
    :initarg :table-oid
    :reader field-description-table-oid)
   (attribute-number
    :initarg :attribute-number
    :reader field-description-attribute-number)
   (type-id
    :initarg :type-id
    :reader field-description-type-id)
   (data-type-size
    :initarg :data-type-size
    :reader field-description-data-type-size)
   (type-modifier
    :initarg :type-modifier
    :reader field-description-type-modifier)
   (format-code
    :initarg :format-code
    :reader field-description-format-code)))

(defclass field-format () ())
(defclass field-text-format (field-format) ())
(defclass field-binary-format (field-format) ())

(defun field-format (field-description)
  (declare (type field-description field-description))
  (ecase (field-description-format-code field-description)
    (0 (make-instance 'field-text-format))
    (1 (make-instance 'field-binary-format))))

(defmethod external-format ((charset charset-utf8))
  (flex:make-external-format :utf8))

(defmethod external-format ((charset charset-iso-8859-1))
  (flex:make-external-format :latin1))

(defmethod external-format ((charset charset-us-ascii))
  (flex:make-external-format :us-ascii))

(defmethod field-format-convert ((vector vector)
				  (charset charset)
				  (format field-binary-format))
  vector)

(defmethod field-format-convert ((vector vector)
				 (charset charset)
				 (format field-text-format))
  (flex:octets-to-string vector :external-format
			 (external-format charset)))


(defclass row-description-message (message)
  ((fields :initarg :fields :reader row-description-fields)))

(defmethod read-message-body ((message-tag (eql #\T))
			      input
			      (charset charset))
  (make-instance
   'row-description-message
   :fields (loop repeat (read-int16 input)
		 collect (make-instance 'field-description
					:name (read-str input charset)
					:table-oid (read-int32 input)
					:attribute-number (read-int16 input)
					:type-id (read-int32 input)
					:data-type-size (read-int16 input)
					:type-modifier (read-int32 input)
					:format-code (read-int16 input)))))





