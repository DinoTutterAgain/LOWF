(defpackage :lowf.router
  (:use :cl)
  (:import-from :alexandria
  		:make-keyword)
  
  (:import-from :lowf.logger
		:log-info)

  (:import-from :lowf.request
		:request-set-captures)

  (:import-from :lowf.response
		:respond-send-file
		:respond-plaintext))

(in-package :lowf.router)

;; internal
(defun starts-with-colon? (input-string)
  "Does input-string start with a :?"<
  (and (> (length input-string) 1)
       (eq (char input-string 0) #\:)))

;; internal
(defun segment-to-name (segment-string)
  "returns the segment-string with the first character removed"
  (subseq segment-string 1))


;; internal
(defun regexify-path (path-segments)
  "take a string like '/users/:id' and return a regex like '^\/users\/[^\/]+\/?$'"

  (labels ((join-path (segments)
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



;;
;; de-router
;;

(defstruct route-path
  original-string
  format-string
  arg-count)

(defparameter *de-route-table* nil)

(export 'reset-de-routing-table)
(defun reset-de-routing-table ()
  (setf *de-route-table* (make-hash-table)))

(export 'route-path-to)
(defun route-path-to (name &rest args)
  (let ((entry (gethash name *de-route-table*)))
    (if entry
	(let ((arg-count (length args))
	      (route-arg-count (route-path-arg-count entry)))
	  (if (eq arg-count
		  route-arg-count)

	      (apply #'format (append (list nil
					    (route-path-format-string entry))
				      args))

	      ;; incorrect number of arguments
	      (error "Incorrect number of arguments for route (wanted ~d, got ~d)"
		     route-arg-count arg-count)))

	(error "A route of name '~a' could not be found" name))))

(defun build-de-route-format-string (path-segments)

  (loop for segment in path-segments
     with arg-count = 0

     if (starts-with-colon? segment)
     collect "~a" into segment-parts
     and do (incf arg-count)
     else
     collect segment into segment-parts

	finally (return (values (if segment-parts
				    (format nil "~{~A~^/~}" segment-parts)
				    "/")
			     arg-count))))

(defun add-de-routing-point (name path-string path-segments)
  (if (gethash name *de-route-table*)
      (error "Route with name '~a' already defined" name))

  (multiple-value-bind (format-string arg-count) (build-de-route-format-string path-segments)
    (setf (gethash name *de-route-table*)
	  (make-route-path :original-string path-string
			   :format-string format-string
			   :arg-count arg-count))))

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
(defun build-routing-table (top-level-route-spec)
  (declare (optimize (debug 3)))
  
  (labels ((process-basic-request (method-type-spec args)	     
	           (let* ((try-path (first args))
		                (controller-method (second args))
		                (route-name (third args))
		                
		                (path-segments (cl-ppcre:split "\/" try-path)))

	             (multiple-value-bind (path-regex pattern-names) (regexify-path path-segments)
		             
		             (when route-name
		               (add-de-routing-point route-name try-path path-segments))
		             
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

(export 'define-route-table)
(defun define-route-table (route-spec)
  (reset-de-routing-table)
  (setf *main-routing-method* (build-routing-table route-spec))
  t)

#|

going to see if i can collect all these down here

TODO:
* more http verbs (and :any)
* not found handler
* static file handler
* make 'build route table' API in to a macro
..- with methods for defining route points
* wrappers: if i redefine a wrapper method, do i need to rebuild the routing table?
* `route-name` so we can 'de-route' in app
* remove `puts`
* fixed path segment value capturing

integration:
* exporting all the right bits
..* integrate in to server
* logging?
* finish static file handler
- some way of printing the routing table?

CAVEAT:
- think about optimisation
- make 'dev' mode be as flexible as possible, in 'prod' mode- be fast

|#
