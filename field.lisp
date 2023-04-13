(uiop:define-package :konnekt/postgres/field
  (:use :common-lisp))

(in-package :konnekt/postgres/field)

;;; Slot documentation taken verbatim from
;;; https://www.postgresql.org/docs/current/protocol-message-formats.html
(defclass field ()
  ((name
    :initarg :name
    :reader field-name
    :type string
    :initform (error "No name given.")
    :documentation "The field name.")
   (table-oid
    :initarg :table-oid
    :reader field-table-oid
    :type (signed-byte 32)
    :initform (error "No table given.")
    :documentation "If the field can be identified as a column of a
    specific table, the object ID of the table; otherwise zero.")
   (attribute-number
    :initarg :attribute-number
    :reader field-attribute-number
    :type (signed-byte 16)
    :initform (error "No attribute-number given.")
    :documentation "If the field can be identified as a column of a
    specific table, the attribute number of the column; otherwise
    zero.")
   (type-oid
    :initarg :type-oid
    :reader field-type-oid
    :type (signed-byte 32)
    :initform (error "No type-oid given.")
    :documentation "The object ID of the field's data type.")))



