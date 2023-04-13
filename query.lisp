(uiop:define-package :konnekt/postgres/query
    (:use :common-lisp))

(in-package :konnekt/postgres/query)

(defclass query ()
  ((string :initarg :string :reader query-string)
   (parameters :initarg :parameters :reader query-parameters)))
