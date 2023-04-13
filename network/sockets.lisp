(uiop:define-package :konnekt/postgres/network/sockets
  (:use :common-lisp
	:konnekt/postgres/network/generics)
  (:export
   #:call-with-socket))

(in-package :konnekt/postgres/network/sockets)

#+sbcl
(defmethod read-vector ((length integer)
			(socket sb-bsd-sockets:socket))
  (sb-bsd-sockets:socket-receive socket nil length
				 :element-type '(unsigned-byte 8)))

#+sbcl
(defmethod write-vector ((vector vector)
			 (socket sb-bsd-sockets:socket))
  (declare (type (vector (unsigned-byte 8) *) vector))
  (sb-bsd-sockets:socket-send socket vector nil)
  (values))

#+sbcl
(defun call-with-socket (function &key host port)
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
			       :type :stream
			       :protocol :tcp)))
    (sb-bsd-sockets:socket-connect socket host port)
    (unwind-protect (funcall function socket)
      (sb-bsd-sockets:socket-close socket))))
