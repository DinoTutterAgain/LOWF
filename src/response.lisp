(defpackage :lowf.response
  (:use :cl)
  (:export :set-not-found-handler
	   :invoke-not-found-handler
	   :set-response-code)
  (:import-from :lowf.html-views
		:wrap-view-in-layout))

(in-package :lowf.response)

(defparameter *not-found-handler* nil)

;; exported
(defun set-not-found-handler (callback)
  (setf *not-found-handler* callback))

;; exported
(defun set-response-code (code)
  (setf (hunchentoot:return-code*) code))

;; exported (for use in server.lisp)
(defun invoke-not-found-handler (request)
  (when *not-found-handler*
    (funcall *not-found-handler* request)))

(export 'respond-html-view)
(defun respond-html-view (request view-contents)
  ;; (maybe set the request/response mime type here?)
  (wrap-view-in-layout view-contents))
