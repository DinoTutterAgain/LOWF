(defpackage :lowf.response
  (:use :cl)
  (:export :respond-plaintext
	   :respond-redirect
	   :respond-html-view
	   :respond-send-file
	   :cookie)

  (:import-from :lowf.logger
		:log-info)
  
  (:import-from :lowf.utils
		:cassoc)
  
  (:import-from :lowf.html-views
		:wrap-view-in-layout))

(in-package :lowf.response)


;; exported
(defun cookie (name value &key expires http-only (path "/"))
  (labels ((format-timestamp (time)
	     (local-time:format-timestring nil
					   time
					   :format '(:short-weekday ", "
						     :day " "
						     :month " "
						     :year " "
						     :hour ":"
						     :min ":"
						     :sec " "
						     :timezone))))
    ;; is there a better way of doing this?
    (concatenate 'string
		 (quri:url-encode-params (list (cons name value)))
		 
		 (if expires (format nil "; Expires=~a" (format-timestamp expires)) "")
		 (if http-only "; HttpOnly" "")
		 (if path (format nil "; Path=~a" path) ""))))

;; (cookie "flash:notice" "the game is real")

;; exported
(defun respond-html-view (view-contents &key (status 200) headers)
  (list status
	headers
	(list (wrap-view-in-layout view-contents))))

(defun set-headers (headers &rest values)
  (let ((out-value (copy-list headers)))
    (labels ((consume-values (value-list)
	       (if value-list
		   (let ((key (first value-list))
			 (value (second value-list)))
		     (setf (getf out-value key) value)
		     (consume-values (cddr value-list)))
		   out-value)))
      (consume-values values))))

;; exported
(defun respond-redirect (to-path &key temporary headers)
  (let ((out-headers (progn
		       (setf (getf headers :location) to-path)
		       headers)))
  
    (list (if temporary 302 301)
	  out-headers
	  (list (format nil "You are being redirected to ~a" to-path)))))

;; exported
(defun respond-plaintext (contents &optional headers)
  (list 200
	headers
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
(defun respond-send-file (base-dir request-path &optional headers)
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


#|
'(define-json-decoder (name)
  (returns-array)
  (returns-object-from
   (decode-fields-to 'user
    (string :name)
    (string :email)
    (string :display-name "display_name")
    (array :permissions ())
    (datetime :created_at)))


  (null?)
  boolean
  integer
  string
  float
  any
  array
  object

  (optional, nullable)

  ;; for objects
  (field :name (:optional) (:nullable) (type :boolean))

  (array (type ...)))
|#
