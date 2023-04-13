(uiop:define-package :konnekt/postgres/database
  (:use :common-lisp
	:konnekt/postgres/big-endian
	:konnekt/postgres/char-encoding
	:konnekt/postgres/connection
	:konnekt/postgres/server-message
	:konnekt/postgres/message-handler
	:konnekt/postgres/client-message
	:konnekt/postgres/octet-buffer)
  (:export
   #:database
   #:call-with-database))

(in-package :konnekt/postgres/database)

(defclass database ()
  ((connection
    :initarg :connection
    :initform (error "No connection given.")
    :type connection
    :reader database-connection
    :documentation "The connection associated with this database instance.")
   (client-encoding
    :initarg :client-encoding
    :initform (make-instance 'char-encoding-us-ascii)
    :type char-encoding
    :accessor database-client-encoding
    :documentation "The current database client encoding.")
   (server-encoding
    :initarg :server-encoding
    :initform (make-instance 'char-encoding-us-ascii)
    :type char-encoding
    :accessor database-server-encoding
    :documentation "The current database server encoding.")))

(defmethod print-object ((database database) stream)
  "Prints the given database object to stream."
  (print-unreadable-object (database stream :type t)
    (format stream "client-encoding: ~A, server-encoding ~A"
	    (database-client-encoding database)
	    (database-server-encoding database))))

(defmethod read-server-message ((database database))
  "Reads a server-message from the given database."
  (let* ((us-ascii (make-instance 'char-encoding-us-ascii))
	 (server-encoding (database-server-encoding database))
	 (connection (database-connection database))
	 ;; Message Header:  5 Bytes
	 ;; First byte contains the message tag.
	 ;; The next 4 bytes contains the message length.
	 ;; The message length includes the 4 bytes of the
	 ;; message length itself.
	 (header-vector (connection-read-octet-vector 5 connection))
	 (header-buffer (make-instance 'octet-buffer :vector header-vector))
	 (message-tag (read-char-using-encoding header-buffer us-ascii))
	 (message-length (read-signed-byte-32 header-buffer))
	 ;; Length of the body is message-length - 4.
	 (message-body (connection-read-octet-vector (- message-length 4)
						     connection)))
    (parse-server-message message-tag message-body server-encoding)))

(defmethod send-client-message ((client-message client-message)
				(database database))
  "Sends the given client-message to the database server.  Does not block."
  (let ((client-encoding (database-client-encoding database)))
    (connection-send-octet-vector
     (write-client-message-to-octet-vector client-message client-encoding)
     (database-connection database))))

(defun call-with-database
    (function &key
		(host #(127 0 0 1))
		(port 5432)
		(database-user (error "No database-user given."))
		database-name)
  "Calls the given function with an established database connection.
Closes the connection after the function returns."
  (call-with-connection
   (lambda (connection)
     (let ((database (make-instance 'database :connection connection)))
       ;; Send startup-message and handle
       (database-send-client-message
	(make-instance 'startup-message
		       :database-user database-user
		       :database-name database-name)
	database)
       (message-loop database (make-instance 'startup-handler))
       (funcall function database)))
   :host host
   :port port))

(defmethod handle-server-message :before ((database database)
					  (message-handler message-handler)
					  (server-message server-message))
  "Print received message to *standard-output*"
  (declare (ignore database))
  (declare (ignore message-handler))
  (print server-message))

(defun find-char-encoding (char-encoding-name)
  (cond ((string= char-encoding-name "UTF8")
	 (make-instance 'char-encoding-utf-8))
	(t (error "Unknown char-encoding: ~A" char-encoding-name))))

(defmethod handle-server-message ((database database)
				  (message-handler message-handler)
				  (parameter-status parameter-status))
  "Handles a parameter-status message."
  (declare (ignore message-handler))
  (let ((name (parameter-status-name parameter-status))
	(value (parameter-status-value parameter-status)))
    (when (string= name "client_encoding")
      (setf (database-client-encoding database)
	    (find-char-encoding value)))
    (when (string= name "server_encoding")
      (setf (database-server-encoding database)
	    (find-char-encoding value)))))

(defmethod handle-server-message ((database database)
				  (message-handler message-handler)
				  (backend-key-data backend-key-data))
  "Handles a backend-key-data message."
  (declare (ignore database))
  (declare (ignore message-handler))
  (declare (ignore backend-key-data)))
