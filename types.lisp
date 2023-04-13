(uiop:define-package :konnekt/postgres/types
  (:use :common-lisp)
  (:export
   #:postgres-type
   #:postgres-string-type
   #:postgres-integer-type
   #:postgres-int4-type))


(in-package :konnekt/postgres/types)

(defgeneric postgres-type (type-id))

(defclass postgres-type ()
  ((id :initarg :type-id :reader type-id)))

(defmethod print-object ((self postgres-type) (stream stream))
  (print-unreadable-object (self stream :type t)
    (format stream "OID: ~A" (type-id self))))

(defmethod postgres-type (type-id)
  (make-instance 'postgres-type :type-id type-id))

(defclass postgres-string-type (postgres-type) ())
;;; varchar string etc...

(defmethod postgres-type ((type-id (eql 1043)))
  (make-instance 'postgres-string-type))

(defclass postgres-integer-type (postgres-type) ())
(defclass postgres-int4-type (postgres-integer-type) ()
  (:default-initargs :type-id 23))

(defmethod postgres-type ((type-id (eql 23)))
  (make-instance 'postgres-int4))



