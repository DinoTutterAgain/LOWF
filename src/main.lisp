(defpackage :lowf
  (:use :cl)
  (:export :run-server)
  (:local-nicknames (:ht :hunchentoot))
  (:import-from :lowf.config
		:config
		:config-int)
  (:import-from :lowf.logger
		:log-info)
  (:import-from :lowf.router
		:route-request))

(in-package :lowf)

(defparameter *app-acceptor* nil)

;;
;; hunchentoot acceptor stuff
;;

(defclass app-acceptor (ht:acceptor)
  ())

;;(defmethod ht:acceptor-log-message ((acceptor app-acceptor) log-level format &rest args)
;;  TODO: logging
;;  )

(defmethod ht:acceptor-dispatch-request ((acceptor app-acceptor) request)
  (let ((method (ht:request-method request))
	(path (ht:request-uri request)))
    
    (log-info "[~s] ~s~%" method path)

    (multiple-value-bind (callback captures) (route-request method path)
      
      (if callback
	  (progn
	    (setf (hunchentoot:aux-request-value :path-captures) captures
		  (hunchentoot:aux-request-value :query-args) query-args)
	    (funcall callback request))

	  
	  (call-next-method)))))
    
;;    (route-request method path)
	      
;;  (setf (ht:header-out "Content-Type") "application/json")
;;  "{ \"count\": 123 }")

;;
;; server functions
;;

(defun make-acceptor ()
  (setf *app-acceptor*
	(or *app-acceptor*
	    (let ((address (config :server_address "0.0.0.0"))
		  (port (config-int :server_port 8002)))
	      
	      (log-info "make-acceptor: address=~s  port=~s" address port)
	      
	      (make-instance 'app-acceptor
			     :port port
			     :address address)))))

(export 'start-server)
(defun start-server ()
  (ht:start (make-acceptor)))

(export 'stop-server)
(defun stop-server ()
  (ht:stop (make-acceptor)))
