(defpackage :lowf.response
  (:use :cl)
  (:export :respond-plaintext
	   :respond-redirect
	   :respond-html-view
	   :respond-send-file)

  (:import-from :lowf.logger
		:log-info)
  
  (:import-from :lowf.utils
		:cassoc)
  
  (:import-from :lowf.html-views
		:wrap-view-in-layout))

(in-package :lowf.response)

;; TODO: headers

;; exported
(defun respond-html-view (view-contents &key (status 200) headers)
  (list status
	headers
	(list (wrap-view-in-layout view-contents))))

;; exported
(defun respond-redirect (to-path &optional temporary)
  (list (if temporary 302 301)
	(list :location to-path)
	(list (format nil "You are being redirected to ~a" to-path))))

;; exported
(defun respond-plaintext (contents)
  (list 200
	nil
	(list contents)))

;;
;; file sending
;;

;; private
(defparameter *extension-to-mimetype*
  '(("css" . "text/css")
    ("ico" . "image/vnd.microsoft.icon")
    ("jpg" . "image/jpeg")
    ("jpeg" . "image/jpeg")
    ("js" . "text/javascript")
    ("png" . "image/png")))

;; private
(defun identify-file-type (path)
  (let ((extension (pathname-type path)))
    (cassoc extension *extension-to-mimetype* t)))

;; exported
(defun respond-send-file (base-dir request-path)
  ;; TODO make is so people can't request files outside of
  ;;    the static-file-directory (verify this?)
  (let ((request-path (if (string= (subseq request-path 0 1) "/")
			  (subseq request-path 1)
			  request-path)))

    (when (> (length request-path) 0)

      (let ((full-path (merge-pathnames request-path base-dir)))

	(when (osicat:file-exists-p full-path)
	  (let ((mime-type (identify-file-type full-path)))
	    (log-info "sending static file ~s (~a)" request-path mime-type)
	    (list 200
		  (list :content-type mime-type)
		  full-path)))))))
