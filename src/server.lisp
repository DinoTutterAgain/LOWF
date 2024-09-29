(defpackage :lowf.server
  (:use :cl)
  (:export :run-server)
  (:local-nicknames (:ht :hunchentoot))
  (:import-from :lowf.config :config))

(in-package :lowf.server)

(defparameter *app-acceptor* nil)

;;
;; hunchentoot acceptor stuff
;;

(defclass app-acceptor (ht:acceptor)
  ())

(defmethod ht:acceptor-dispatch-request ((acceptor app-acceptor) request)
  (format t "acceptor-dispatch-request: uri=~s~%" (ht:request-uri request))
  (setf (ht:header-out "Content-Type") "application/json")
  "{ \"count\": 123 }")

;;
;; server functions
;;

(export 'get-acceptor)
(defun get-acceptor ()
  (setf *app-acceptor* (or *app-acceptor*
			   (let ((address (config :server-address))
				 (port (config :sewrver-port)))
			     (format t "make-acceptor: address=~s  port=~s~%" address port)
  
			     (make-instance 'app-acceptor
					    :port port
					    :address address)))))

(export 'start-server)
(defun start-server ()
  (ht:start (get-acceptor)))

(export 'stop-server)
(defun stop-server ()
  (ht:stop (get-acceptor)))
