(defpackage :lowf.router
  (:use :cl)
  (:local-nicknames (:x :alexandria))

  (:import-from :lowf.logger
		            :log-info)

  (:import-from :lowf.request
		            :request-set-captures)

  (:import-from :lowf.response
		            :respond-send-file
		            :respond-plaintext))

(in-package :lowf.router)


(defun find-alist-value (key alist)
  "looks up KEY in ALIST and returns (VALUES <value> T)
  if it exists or NIL if not. Used to determine if
  the value in the alist is present (and maybe NIL) or
  if not present."
  
  (labels ((walk-value-list (prop-list)
             (when prop-list
               (let* ((prop (first prop-list))
                      (prop-key (car prop)))
                 
                   (if (eq key prop-key)
                       (values (cdr prop)
                               t)
                       (walk-value-list (rest prop-list)))))))
    
    (walk-value-list alist)))


;;
;; de-router
;;

(defparameter *de-routing-table* nil)

(defstruct route-path
  original-string
  path-chunks
  arg-count)

;; internal
(defun chunk-path (path-string)
  (labels ((string-or-keyword (input)
             (if (and (> (length input) 1)
                      (string-equal ":" (subseq input 0 1)))
                 (x:make-keyword (string-upcase (subseq input 1)))
                 input))
           
           (generate-segment (segment the-rest previous-keyword?)

             (if (keywordp segment)
                 (cons segment the-rest)

                 (let ((first-the-rest (first the-rest)))
                   (cons (concatenate 'string
                                      (if previous-keyword? "/" "")
                                      segment
                                      (if first-the-rest "/" "")
                                      (if (stringp first-the-rest)
                                          first-the-rest
                                          ""))
                         
                         (if (stringp first-the-rest)
                             (cdr the-rest)
                             the-rest)))))
           
           
           (consume-segment (path-segments previous-keyword?)
             
             (if path-segments
                 (let ((segment (string-or-keyword (first path-segments))))
                   (generate-segment segment
                                     (consume-segment (rest path-segments)
                                                      (keywordp segment))
                                     previous-keyword?) ))))
    
    (let ((path-segments (cl-ppcre:split "\/" path-string)))  
      (consume-segment path-segments nil))))

;; internal
(defun build-derouting-table (route-spec)
  (let ((route-table (make-hash-table)))
    (labels ((walk-route-spec (spec-level)
               (loop for spec in spec-level
                     do (let ((type (first spec)))
                          (cond ((eq type :wrap)
                                 (walk-route-spec (cddr spec)))

                                ((or (eq type :get)
                                     (eq type :post)
                                     (eq type :put)
                                     (eq type :any))
                                     
                                 (x:if-let (name (fourth spec))
                                   ;; IDEA: warn user if route name is used multiple times?
                                   (setf (gethash name route-table)
                                         (let ((chunks (chunk-path (second spec))))
                                           (make-route-path :original-string (second spec)
                                                            :path-chunks chunks
                                                            :arg-count (count-if #'keywordp chunks))))))
                                ;; NO-OP
                                (t nil))))))
      
                                

      (walk-route-spec route-spec))
    route-table))


(export 'route-path-to)
(defun route-path-to (name &rest args)
  (x:if-let (route (gethash name *de-routing-table*))
    (let ((all-args (x:plist-alist args)))
      
      (with-slots (arg-count original-string path-chunks) route
        (with-output-to-string (output)
          (if (zerop arg-count)
              (princ original-string output)
              
              (loop for chunk in path-chunks ;; (this coule be more FP)
                    do (if (stringp chunk)
                           (princ chunk output)

                           (progn
                             (multiple-value-bind (value found) (find-alist-value chunk all-args)
                               (when found
                                 (princ (quri:url-encode (princ value)) output)
                                 (setf all-args (remove-if #'(lambda (cell-key) (eq cell-key chunk))
                                                           all-args
                                                           :key #'car))))))))

          (when all-args
            (princ "?" output)
            (princ (quri:url-encode-params (mapcar #'(lambda (cell) (cons (string-downcase (string (car cell)))
                                                                          (cdr cell)))
                                                   all-args))
                   output)))))))

;;
;; wrapper logic
;;

;; internal
(defparameter *wrapper-db* (make-hash-table))

;; internal
(defun assign-wrapper (name wrapper-method)
  (setf (gethash name *wrapper-db*) wrapper-method))

(export 'define-wrapper)
(defmacro define-wrapper ((name next-method-var request-method-var request-path-var) &body body)
  `(assign-wrapper ,name
		   #'(lambda (,next-method-var ,request-method-var ,request-path-var)
		       (declare (ignorable ,next-method-var ,request-method-var ,request-path-var))
		       ,@body)))

;; internal
(defun find-wrapper (name)
  (gethash name *wrapper-db*))

(export 'pass-request-on)
(defun pass-request-on (next-method request-method request-path)
  (funcall next-method request-method request-path))


;;
;; routing table logic
;;

;; internal
(defun regexify-path (path-segments)
  "take a string like '/users/:id' and return a regex like '^\/users\/[^\/]+\/?$'"

  (labels ((starts-with-colon? (input-string)
             (and (> (length input-string) 1)
                  (eq (char input-string 0) #\:)))

           (segment-to-name (segment-string)
             (subseq segment-string 1))
           
           (join-path (segments)
	           (format nil "^~{~A~^\\/~}\\/?$" segments)))

    (loop
          for segment in path-segments

          if (starts-with-colon? segment)
          collect "([^\\/]*)" into output-segments
          and collect (segment-to-name segment) into output-names

          else
          collect segment into output-segments

          finally (return (values (join-path output-segments)
			                            output-names)))))

;; internal
(defun build-routing-table (top-level-route-spec)
  (declare (optimize (debug 3)))
  
  (labels ((process-basic-request (method-type-spec args)	     
	           (let* ((try-path (first args))
		                (controller-method (second args))
		                
		                (path-segments (cl-ppcre:split "\/" try-path)))

	             (multiple-value-bind (path-regex pattern-names) (regexify-path path-segments)
		             #'(lambda (request-method request-path)
		                 
		                 (if (or (eq request-method :any)
			                       (eq request-method method-type-spec))
			                   
			                   (multiple-value-bind (has-match captures) (cl-ppcre:scan-to-strings path-regex request-path)
			                     (when has-match
                             ;; FIXME: set back the method and path
			                       (request-set-captures pattern-names captures)
			                       (log-info "~s ~s" method-type-spec try-path)
			                       (funcall controller-method))))))))
	         
	         (process-wrap (args)		     
	           (let* ((wrapper-name (first (first args)))		    
		                (inner-spec-level (rest args))
		                (next-function (walk-route-spec-for-level inner-spec-level)))

	             ;; a quick sanity test when defining the routing table
	             (unless (find-wrapper wrapper-name)
		             (error "Can't find wrapper named ~s" wrapper-name))
	             
	             #'(lambda (request-method request-path)
		               (let ((wrapper-method (or (find-wrapper wrapper-name)
					                                   (error "Can't find wrapper named ~s" wrapper-name))))
		                 (funcall wrapper-method next-function request-method request-path)))))

	         (process-not-found (args)
	           (let ((not-found-handler (first args)))
	             
	             #'(lambda (request-method request-path)
		               (log-info "~s ~s (Not Found)" request-method request-path)
		               (funcall not-found-handler))))

	         (process-static-files (args)
	           (let ((base-dir (first args)))
	             
	             #'(lambda (request-method request-path)
		               (if (eq request-method :get)
		                   (respond-send-file base-dir request-path)))))
	         

	         (walk-route-spec-for-level (route-spec)
	           (let ((routing-functions
		                (mapcar #'(lambda (route-definition)
				                        (let ((type (first route-definition)))
				                          (cond ((find type '(:get :post :put :delete :head :patch :any))
					                               (process-basic-request type (cdr route-definition)))
					                              
					                              ((eq type :wrap) (process-wrap (cdr route-definition)))
					                              ((eq type :not-found) (process-not-found (cdr route-definition)))
					                              ((eq type :static-files) (process-static-files (cdr route-definition)))
					                              
					                              (t (error "Unknown route verb ~s" type)))))
			                      route-spec)))
	             
	             #'(lambda (request-method request-path)
		               (loop for try-route-entry in routing-functions
			                   for result = (funcall try-route-entry request-method request-path)
			                   if result return result)))))
	  
	  (walk-route-spec-for-level top-level-route-spec)))

(defparameter *main-routing-method*
  #'(lambda (method path)
      (respond-plaintext (format nil "No routing table defined! (~s ~s)" method path))))

(export 'dispatch-request-for-routing)
(defun dispatch-request-for-routing (method path)
  (funcall *main-routing-method* method path))

;;
;;

(defparameter *original-route-spec* nil)

(export 'define-route-table)
(defun define-route-table (route-spec)
  (setf *original-route-spec* route-spec
        *main-routing-method* (build-routing-table route-spec)
        *de-routing-table* (build-derouting-table route-spec))
  t)

(export 'print-routes)
(defun print-routes ()
  ;; very crude
  (format t "~s~%" *original-route-spec*))


  
#|
(define-wrapper (:demo-wrapper next-method req res)
  (pass-request-on next-method req res))

(define-route-table
  `((:get  "/" act-on-root :root)
    (:get  "/about" act-on-about :about)
    (:get  "/cookie/poke" act-on-poke-cookie)
    (:wrap (:demo-wrapper)
      (:get  "/todo/new" act-on-new-item :new-item)
      (:post "/todo/new" act-do-create-item  :create-item)
      (:get  "/todo/:id" act-on-show-item :show-item))

    (:static-files ,(merge-pathnames "app/public/"
                                     (osicat:current-directory)))
    ;; must be last
    (:not-found act-on-not-found)))

|#

