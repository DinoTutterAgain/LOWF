(defpackage :lowf.request
  (:use :cl)
  (:local-nicknames (:x :alexandria))
  (:import-from :lowf.utils
		            :cassoc)

  (:export :with-post-parameters
	         :path-capture-value
	         :path-capture-value-integer
	         :with-request
	         :request-method
	         :request-path
	         :request-set-captures
	         :request-headers
	         :request-all-cookies
           :request-path-params
           :request-path-arg
           :request-path-arg-number
	         :www-form-params
           :request-local-args
           :request-local-arg))

(in-package :lowf.request)

(defparameter *request* nil)

;; exported
(defmacro with-request ((environment) &body body)
  `(let ((*request* ,environment))
     ,@body))

;; exported
(defun request-method ()
  (getf *request* :request-method))

;; exported
(defun request-path ()
  (getf *request* :path-info))

;; exported
(defun request-local-args ()
  (getf *request* :locals))

;; exported
(defun request-local-arg (name)
  (x:when-let (locals (getf *request* :locals))
    (gethash name locals)))

;; exported
(defun (setf request-local-arg) (value name)
  (labels ((request-locals ()
             (setf (getf *request* :locals)
                   (or (getf *request* :locals)
                       (make-hash-table)))))
    (setf (gethash name (request-locals))
          value)))

;; TODO: a macro like (with-request-local-args (name name name) ...

;; exported
(defun request-headers ()
  (getf *request* :headers))

(defun request-all-cookies ()
  (x:if-let (cookie (gethash "cookie" (request-headers)))
    (mapcar #'(lambda (nibble)
                (quri:url-decode-params nibble))
            (ppcre:split "; " cookie))))

(defun request-path-params ()
  (x:if-let (param-string (getf *request* :query-string))
    (quri:url-decode-params param-string)))

(defun request-path-arg (name &optional default)
  (or (cassoc name
              (request-path-params)
              t)
      default))

(defun request-path-arg-number (name &optional default)
  (or (parse-integer (request-path-arg name "") :junk-allowed t)
      default))
  
;; used when routing
(defun request-set-captures (path-segment-names path-capture-values)
  (let ((path-captures
	 (loop
	       for capture-value across path-capture-values
	       for name in path-segment-names
	       collect (cons (x:make-keyword (string-upcase name))
			     capture-value))))

    (and (setf (getf *request* :path-captures) path-captures)
	 nil)))


;; exported
(defun path-capture-value (id)
  (x:if-let (captures (getf *request* :path-captures))
    (cassoc id captures)))

;; exported
(defun path-capture-value-integer (id)
  (parse-integer (or (path-capture-value id)
		     "")
		 :junk-allowed t))

;; exported
(defun www-form-params ()
  (x:if-let (raw-body (getf *request* :raw-body))
    (quri:url-decode-params
     (babel:octets-to-string
      (x:read-stream-content-into-byte-vector
       raw-body)))))




#|
FIXME: this could be useful if made more generic so it could pull out local values
from a-lists or p-lists and moved in to utils(?)

;; exported
(defmacro with-post-parameters ((&rest parameters-spec) request-arg &body body)
  "Pulls POST arguments out of REQUEST-ARG and assigns them to arguments like WITH-SLOTS:
  (with-post-parameters ((arg-1 \"arg_1\") (arg-2 \"arg_2\") arg-3 ...) request-arg
      <body>)
  "
  (let ((params-arg (gensym "params")))

    (labels ((labelify (param-spec)
	       (if (symbolp param-spec)
		   ;; just pull 'arg with key "ARG"
		   `(,param-spec (cdr (find ,(string param-spec) ,params-arg :key #'car :test #'string=)))

		   (let ((arg-name (first param-spec))
			 (arg-key (second param-spec)))
		     `(,arg-name (cdr (find ,arg-key ,params-arg :key #'car :test #'string=)))) )))

      `(let ((,params-arg (hunchentoot:post-parameters ,request-arg)))
	 (declare (ignorable ,params-arg))
	 (symbol-macrolet ,(mapcar #'labelify parameters-spec)

	   ,@body)))))
|#
