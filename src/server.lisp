(defpackage :lowf.server
  (:use :cl)
  (:export :run-server)
  (:import-from :lowf.config
		:config
		:config-int)

  (:import-from :lowf.logger
		:log-info)

  (:import-from :lowf.router
		:dispatch-request-for-routing)

  (:import-from :lowf.request
		:with-request
		:request-method
		:request-path))

(in-package :lowf.server)

;; internal
(defparameter *app-setup-hook* nil)

;; internal
(defparameter *woo-server* nil)

;; internal
(defun handle-woo-request (env)
  (with-request (env)
    (dispatch-request-for-routing (request-method) (request-path))))

;;
;; server functions
;;

(export 'define-server-pre-start)
(defmacro define-server-pre-start (&body body)
  `(setf *app-setup-hook*
	 #'(lambda ()
	     ,@body)))

(export 'run-server)
(defun run-server ()
  (let ((host (config :listen-host "127.0.0.1"))
	(port (config-int :listen-port 8002)))
    (log-info "Running server at http://~a:~d" host port)
    (if *app-setup-hook*
	(funcall *app-setup-hook*))

    (setf *woo-server*
	  (woo:run #'handle-woo-request
		   :port port
		   :address host
		   :worker-num 1))))

;; (defun stop-server ()
;;  FIXME: we need (run-server) to start a background task and
;;         capture the server reference for this to work >.<*
;;  (woo:stop ...))

#|

(export 'try-request)
(defun try-request (method path &key body params)
  (let ((fake-request ()))
    ;; TODO
    ))
|#
