(defpackage :lowf
  (:use :cl)
  (:export :run-server)
  (:local-nicknames (:ht :hunchentoot)))

(in-package :lowf)

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

(defun make-acceptor (port address)
  (format t "make-acceptor: address=~s  port=~s~%" address port)
  
  (make-instance 'app-acceptor
		 :port port
		 :address address))

(defun run-server ()
  (setf *app-acceptor* (make-acceptor 3002 "localhost"))
  
  (unwind-protect
       (progn
	 (ht:start *app-acceptor*)
	 (loop do (sleep 60))) ;; idle
    
    (progn
      (format t "run-server: cleanup ~%")
      (ht:stop *app-acceptor*))))
