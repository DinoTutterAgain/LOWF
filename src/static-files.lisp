(defpackage :lowf.static-files
  (:use :cl)
  (:import-from :lowf.logger
		:log-info)
  (:import-from :lowf.utils
		:cassoc))


(in-package :lowf.static-files)

(defparameter *extension-to-mimetype*
  '(("css" . "text/css")
    ("ico" . "image/vnd.microsoft.icon")
    ("jpg" . "image/jpeg")
    ("jpeg" . "image/jpeg")
    ("js" . "text/javascript")
    ("png" . "image/png")))

(defparameter *static-file-directory* nil)

(defun identify-file-type (path)
  (let ((extension (pathname-type path)))
    (cassoc extension *extension-to-mimetype* t)))

(export 'set-public-directory)
(defun set-public-directory (public-path)
  (log-info "Setting public folder: ~s" public-path)
  (setf *static-file-directory*	public-path))

(export 'maybe-serve-static-file)
(defun maybe-serve-static-file (path)
  ;; TODO make is so people can't request files outside of
  ;;    the static-file-directory
  (let ((path (if (string= (subseq path 0 1) "/")
		  (subseq path 1))))

    (when (and *static-file-directory*
	       (> (length path) 0))

      (let ((full-path (merge-pathnames path *static-file-directory*)))

	(when (osicat:file-exists-p full-path)
	  (let ((mime-type (identify-file-type full-path)))
	    (log-info "sending static file ~s (~a)" path mime-type)
	    (list 200
		  (list :content-type mime-type)
		  full-path)))))))
