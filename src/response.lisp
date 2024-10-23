(defpackage :lowf.response
  (:use :cl))

(in-package :lowf.response)

(defun set-not-found-handler (callback)
  (setf (slot-value (make-acceptor) 'not-found-handler) callback))

(export 'define-not-found)
(defmacro define-not-found (&body body)
  `(set-not-found-handler #'(lambda ()
			      ,@body)))
