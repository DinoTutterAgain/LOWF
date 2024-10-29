(defpackage :lowf.request
  (:use :cl)
  (:export :with-post-parameters
	   :path-captures
	   :path-capture-value
	   :path-capture-integer
	   :current-request))

(in-package :lowf.request)

;; (defun key-lookup (key-string dict-list)
;;  (cdr (find key-string dict-list :key #'second :test 'string=)))

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

;; exported
(defun path-captures ()
  (hunchentoot:aux-request-value :path-captures))

;; exported
(defun path-capture-value (key)
  (cdr (find key (path-captures) :key #'car)))

;; exported
(defun path-capture-integer (key)
  (parse-integer (path-capture-value key) :junk-allowed t))

;; exported
(defun current-request ()
  ;; this is a fudge whilst i figure out the API 
  (when (boundp 'hunchentoot:*request*)
    hunchentoot:*request*))
