(defpackage :lowf.config
  (:use :cl)
  (:import-from :lowf.utils
		:keywordarize-alist
		:cassoc))

(in-package :lowf.config)

(defparameter *config-table* nil)
(defconstant +CONFIG-LEADER+ "CONFIG_")

(defun is-config-name? (name)
  (if (>= (length name)
	  (1+ (length +CONFIG-LEADER+)))
      (string= (subseq name 0 (length +CONFIG-LEADER+))
	       +CONFIG-LEADER+)))

(defun trim-config-name (name)
  (let ((leader-len (length +CONFIG-LEADER+)))

    (if (>= (length name)
	    (1+ leader-len))
	(subseq name leader-len))))

(defun config-table ()
  (setf *config-table*
	(or *config-table*
	    (keywordarize-alist 
	     (loop for (name . value) in (osicat:environment)
		if (is-config-name? name)
		collect (cons (trim-config-name name)
			      value))))))

(export 'config)
(defun config (name &optional default)
  (or (cassoc name (config-table))
      default))

(export 'config-int)
(defun config-int (name &optional (default 0))
  (let ((value (config name)))
    (if value (parse-integer value :junk-allowed t)
	default)))

