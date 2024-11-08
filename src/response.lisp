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

;; exported (for use in server.lisp)
(defun invoke-not-found-handler ()
  (when *not-found-handler*
    (funcall *not-found-handler*)))

(export 'respond-html-view)
(defun respond-html-view (view-contents &key (status 200) headers)
  (list status
	headers
	(list (wrap-view-in-layout view-contents))))

(defun respond-redirect (to-path &optional permanent)
  (list (if permanent 302 301)
	(list :location to-path)
	(list (format nil "You are being redirected to ~a" to-path))))

(defun respond-file (path)
  (list 200
	nil
	path))

;; sendfile?

