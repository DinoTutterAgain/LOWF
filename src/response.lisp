(defpackage :lowf.response
  (:use :cl)
  (:local-nicknames (:html :lowf.html-view.tags))
  (:import-from :lowf.html-views
		:respond-html-view
		:wrap-view-in-layout)
  ;;(:import-from :
  (:export :define-not-found
	   :respond-not-found-html
	   :respond-redirect))

(in-package :lowf.response)

(defparameter *not-found-handler* nil)

;; exported
(defmacro define-not-found ((message-var) &body body)
  `(setf *not-found-handler* #'(lambda (,message-var)
				 (declare (ignorable ,message-var))
				 ,@body)))

;; exported
(defun respond-not-found-html (message)
  (setf (hunchentoot:return-code*) 404)
  (respond-html-view (funcall *not-found-handler* message)))

;; default handler
(define-not-found (content)
  (list
   (html:h1 () "404 / Not found")
   (if content
       (html:p () (format nil "Problem: ~a" content)))))
  

;; exported
(defun respond-redirect (path &key moved-permanently)
  (hunchentoot:redirect path :code (if moved-permanently 302 301)))


(export 'respond-html-view)
(defun respond-html-view (request view-contents)
  ;; (maybe set the request/response mime type here?)
  (wrap-view-in-layout view-contents))

;;  (render-html (funcall *layout-function* request view-contents)))
