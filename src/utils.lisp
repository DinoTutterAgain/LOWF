(defpackage :lowf.utils
  (:use :cl))

(in-package :lowf.utils)

(export 'make-keyword)
(defun make-keyword (name)
  "take a string, return a keyword"
  (intern (string-upcase name)
	  "KEYWORD"))

(export 'path-to)
(defun path-to (&optional file)
  (merge-pathnames (or file "")
		   (osicat:current-directory)))


(export 'cassoc)
(defun cassoc (item alist &optional slow)
  "(cdr (assoc item alist))"
  (cdr (assoc item alist :test (if slow #'equal #'eq))))

(export 'keywordarize-alist)
(defun keywordarize-alist (list)
  (loop for (name . value) in list
     collect (cons (make-keyword (string-upcase name))
		   value)))
