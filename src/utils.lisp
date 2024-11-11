(defpackage :lowf.utils
  (:use :cl)
  (:import-from :alexandria
		:make-keyword))

(in-package :lowf.utils)

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

(export 'present?)
(defun present? (thing)
  "returns T if THING is not NIL"
  (not (null thing)))
