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


(export 'find-alist-value)
(defun find-alist-value (key alist &optional (test #'eq))
  "looks up KEY in ALIST and returns (VALUES <value> T)
  if it exists or NIL if not. Used to determine if
  the value in the alist is present (and maybe NIL) or
  if not present."
  (labels ((walk-value-list (prop-list)
             (when prop-list
               (let* ((prop (first prop-list))
                      (prop-key (car prop)))
                 
                   (if (funcall test key prop-key)
                       (values (cdr prop)
                               t)
                       (walk-value-list (rest prop-list)))))))
    
    (walk-value-list alist)))

(export 'keywordarize-alist)
(defun keywordarize-alist (list)
  (loop for (name . value) in list
     collect (cons (make-keyword (string-upcase name))
		   value)))

(export 'present?)
(defun present? (thing)
  "returns T if THING is not NIL"
  (not (null thing)))
