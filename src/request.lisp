(defpackage :lowf.request
  (:use :cl)
  (:export :with-post-parameters
	   :path-capture-value
	   :path-capture-value-integer
	   :with-request
	   :request-method
	   :request-path
	   :request-set-captures)
  
  (:import-from :lowf.utils
		:find-in-list))

(in-package :lowf.request)

(defparameter *request* nil)

(defun decode-environment (environment)
  environment)

;;(export 'with-request)
(defmacro with-request ((environment) &body body)
  `(let ((*request* ,environment))
     ,@body))

(defun request-method ()
  (find-in-list :request-method *request*))

(defun request-path ()
  (find-in-list :path-info *request*))

;; used when routing
(defun request-set-captures (path-captures)
  (and (setf *request* (concatenate 'list
				    *request*
				    (list :path-captures path-captures)))
       nil))
  
;; exported
(defun path-capture-value (id request)
  (let ((captures (hunchentoot:aux-request-value :path-captures request)))
    (and captures
	 (cdr (find id captures :test 'eq :key 'car)))))

;; exported
(defun path-capture-value-integer (id request)
  (parse-integer (or (path-capture-value id request)
		     "")
		 :junk-allowed t))
      

;; exported
(defmacro with-post-parameters ((&rest parameters-spec) request-arg &body body)
  "Pulls POST arguments out of REQUEST-ARG and assigns them to arguments like WITH-SLOTS"
  #|
  (with-post-parameters ((arg-1 "arg_1") (arg-2 "arg_2") arg-3 ...) request-arg
  <body>)
  |#
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
