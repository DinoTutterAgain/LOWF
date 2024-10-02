(defpackage :lowf
  (:use :cl)
  (:export :run-server)
  (:local-nicknames (:ht :hunchentoot)
		    (:html :lowf.html-view.tags))
  (:import-from :lowf.config
		:config
		:config-int)
  (:import-from :lowf.logger
		:log-info)
  (:import-from :lowf.router
		:route-request
		:define-route-table
		:route))

(in-package :lowf)

(defparameter *app-acceptor* nil)

;;
;; app demo stuff (to be moved)
;;

(defmacro with-layout ((title) &body content)
  `(list
    (html:doctype "html")
    (html:html ()
      (html:head ()
	(html:title () ,title)
	(html:meta :property "og:site_name" :content "LOWF demo site")
	(html:meta :property "og:type" :content "website")
	(html:meta :property "og:description" :content "A sample website from build-site")
	(html:meta :charset "UTF-8"))
      (html:main ()
	,@content)
      (html:footer ()
	(html:p () "Copyright &copy; 2024, Me Corp")))))

(defun render-root ()
  (with-layout ("demo page")
    (html:h1 () "Hello!")))

(defun render-show ()
  (with-layout ("Show thing page")
    (html:h1 () "Showing a thing here")))

(defun act-on-index (request)
  (lowf.html-views:respond-html-view request (render-root)))

(defun act-on-show (request)
  (lowf.html-views:respond-html-view request (render-show)))

(define-route-table
  (route :get :root "/" #'act-on-index)
  (route :get :show "/image/:id" #'act-on-show))


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
	    (setf (hunchentoot:aux-request-value :path-captures) captures)
		  ;;(hunchentoot:aux-request-value :query-args) query-args) -- use (ht:get-parameters*) instead!!
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
