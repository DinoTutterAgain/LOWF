(defpackage :lowf.server
  (:use :cl)
  (:export :run-server)
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
		:invoke-not-found-handler)

  (:import-from :lowf.request
		:with-request
		:request-method
		:request-path
		:request-set-captures)

  (:import-from :lowf.static-files
		:maybe-serve-static-file))

(in-package :lowf.server)

(defparameter *app-setup-hook* nil)
(defparameter *woo-server* nil)

(defun handle-woo-request (env)
  (with-request (env)
    (multiple-value-bind (callback captures) (route-request (request-method) (request-path))
      (request-set-captures captures)

      (or (if callback (funcall callback))
	  (maybe-serve-static-file (request-path))
	  (invoke-not-found-handler)))))


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
