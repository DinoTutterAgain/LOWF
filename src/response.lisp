(defpackage :lowf.response
  (:use :cl)
  (:export :set-not-found-handler
	   :invoke-not-found-handler
	   :set-response-code))

(in-package :lowf.response)

(defparameter *not-found-handler* nil)

;; exported
(defun set-not-found-handler (callback)
  (setf *not-found-handler* callback))

;; exported
(defun set-response-code (code)
  (setf (hunchentoot:return-code*) code))

;; exported
;;(defmacro define-not-found (&body body)
;;  `(set-not-found-handler #'(lambda ()
;;			      ,@body)))

;; exported (for use in server.lisp)
(defun invoke-not-found-handler (request)
  (when *not-found-handler*
    (funcall *not-found-handler* request)))
