(defpackage :lowf.server
  (:use :cl)
  (:export :run-server)
  (:local-nicknames (:ht :hunchentoot))
  (:import-from :lowf.config
		:config
		:config-int)
  
  (:import-from :lowf.utils
		:present?)
  
  (:import-from :lowf.logger
		:log-info)
  
  (:import-from :lowf.router
		:route-request)
  
  (:import-from :lowf.response
		:invoke-not-found-handler))

(in-package :lowf.server)

(defparameter *app-acceptor* nil)
(defparameter *app-setup-hook* nil)

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
	    (setf (hunchentoot:aux-request-value :path-captures) captures)
		  ;;(hunchentoot:aux-request-value :query-args) query-args) -- use (ht:get-parameters*) instead!!
	    (funcall callback request))


	  (call-next-method)))))

(defmethod ht:acceptor-status-message ((acceptor app-acceptor) http-status-code &key)
  (log-info "acceptor-status-message http-status-code=~s" http-status-code)

  (or (and (= http-status-code 404)
	   (invoke-not-found-handler nil))
      (call-next-method)))  

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

(export 'define-server-pre-start)
(defmacro define-server-pre-start (&body body)
  `(setf *app-setup-hook*
	 #'(lambda ()
	     ,@body)))



(export 'set-public-directory)
(defun set-public-directory (public-path)
  (log-info "Setting public folder: ~s" public-path)
  (setf (ht:acceptor-document-root (make-acceptor))
	public-path))

(export 'start-server)
(defun start-server ()
  (if *app-setup-hook* (funcall *app-setup-hook*))
  (ht:start (make-acceptor)))

(export 'stop-server)
(defun stop-server ()
  (ht:stop (make-acceptor)))

(export 'try-request)
(defun try-request (method path)
  (let ((fake-request (make-instance 'ht:request)))
    ;; TODO
    ))
