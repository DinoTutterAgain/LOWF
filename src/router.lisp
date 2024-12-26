(defpackage :lowf.router
  (:use :cl)
  (:local-nicknames (:x :alexandria))

  (:import-from :lowf.utils
                :cassoc)
  
  (:import-from :lowf.logger
		            :log-info)

  (:import-from :lowf.request
		            :request-set-captures)

  (:import-from :lowf.response
		            :respond-send-file
		            :respond-plaintext))

(in-package :lowf.router)


;; internal
(defun regexify-path (path-segments)
  "take a string like '/users/:id' and return a regex like '^\/users\/[^\/]+\/?$'"

  (labels ((starts-with-colon? (input-string)
             "Does input-string start with a :?"
             (and (> (length input-string) 1)
                  (eq (char input-string 0) #\:)))

           (segment-to-name (segment-string)
             "returns the segment-string with the first character removed"
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



;;
;; de-router
;;

(defstruct route-path
  original-string
  path-chunks
  format-string ;; needed?
  args-by-position
  arg-count)

(defparameter *de-route-table* nil)

(export 'reset-de-routing-table)
(defun reset-de-routing-table ()
  (setf *de-route-table* (make-hash-table)))

(defun find-arg-value (key value-list)
  
  (labels ((walk-value-list (value-list out-list)
             (when value-list
               (let ((value-key (first value-list)))
                 (if (eq value-key key)
                     (values (second value-list)
                             out-list
                             t)
                     (walk-value-list (cddr value-list) out-list))))))
    (walk-value-list value-list nil)))

(export 'route-path-to)
(defun route-path-to (name &rest args)
  (x:if-let (entry (gethash name *de-route-table*))
    (with-slots (original-string format-string args-by-position arg-count) entry
      
      (if (zerop arg-count)
          ;; TODO: append the rest the args as params
          original-string

          (labels ((find-args-by-position (arg-specs args found-args other-args)
                     (let ((current-arg (first arg-specs)))
                       (multiple-value-bind (value found) (find-arg-value current-arg args)
                         (if found
                             (let ((new-args (append found-args (cons value nil))))
                               
                     ))

            (find-args-by-position args-by-position args nil nil))))

;;          (let ((arg-values-by-position (mapcar #'(lambda (arg-name)
;;                                                    (getf args arg-name))
;;                                                args-by-position)))
            
;;            arg-values-by-position)))

	  (error "A route of name '~a' could not be found" name)))

(defun --------route-path-to (name &rest args)
  (x:if-let (entry (gethash name *de-route-table*))
	  (let ((route-arg-count (route-path-arg-count entry)))
      
      (if (zerop route-arg-count)
          (route-path-original-string entry)
          
          (let ((arg-count (length args)))
	          (if (eq arg-count
		                route-arg-count)

	              (apply #'format (append (list nil
					                                    (route-path-format-string entry))
				                                args))

	              ;; incorrect number of arguments
	              (error "Incorrect number of arguments for route (wanted ~d, got ~d)"
		                   route-arg-count arg-count)))))

	  (error "A route of name '~a' could not be found" name)))


(export 'append-params) ;; a bit cheap-n-dirty but it'll work
(defun append-params (route-string &rest params)
  (let ((param-string (quri:url-encode-params (mapcar #'(lambda (prop)
                                                          (cons (string-downcase (car prop))
                                                                (cdr prop)))
                                                      (x:plist-alist params)))))
    (if (> (length param-string) 0)
           (format nil "~a?~a" route-string param-string)
           route-string)))

(defun build-de-route-format-string (path-segments)

  (loop for segment in path-segments
        with arg-count = 0

        if (starts-with-colon? segment)
        collect "~a" into segment-parts
        and collect (x:make-keyword (string-upcase (subseq segment 1))) into segment-names
        and do (incf arg-count)
        else
        collect segment into segment-parts

	      finally (return (values (if segment-parts
				                            (format nil "~{~A~^/~}" segment-parts)
				                            "/")
			                          arg-count
                                segment-names))))

(defun add-de-routing-point (name path-string path-segments)
  (if (gethash name *de-route-table*)
      (error "Route with name '~a' already defined" name))

  (multiple-value-bind (format-string arg-count arg-names) (build-de-route-format-string path-segments)
    (setf (gethash name *de-route-table*)
	        (make-route-path :original-string path-string
			                     :format-string format-string
			                     :arg-count arg-count
                           :args-by-position arg-names))))

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







TODO: try breaking the URL up into blocks like
URL /books/:bood-id/chapter/:chapter-id/paragraph/:paragraph-id
-> ("books" :BOOK-ID "chapter" :CHAPTER-ID "paragraph" :PARAGRAPH-ID)

then when call route-path-to
1. open an output string stream
2. iterate over the entry chunks
3.   write the plain segments out as normal
4.   lookup all the KEYWORD segment in the param args

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                ;;
;;   i am going to focus on rails for a sec and maybe get a job   ;;
;;                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *routing-table-x* (make-hash-table))

(defstruct route-path-x
  original-string
  path-chunks
  arg-count)

(defun chunk-path (path-string)
  (let ((path-segments (cl-ppcre:split "\/" path-string)))
    
    (labels ((string-or-keyword (input)
               (if (and (> (length input) 1)
                        (starts-with-colon? input))
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
                             
      (consume-segment path-segments nil))))

(defun build-route-path (path)
  (let ((chunks (chunk-path path)))
    (make-route-path-x :original-string path
                       :path-chunks chunks
                       :arg-count (count-if #'keywordp chunks))))

(defun register-route (name path)
  (setf (gethash name *routing-table-x*)
        (build-route-path path)))


(defun print-routes-x ()
  (loop for path-name being the hash-keys of *routing-table-x*
        do (format t "[~s] ~s~%" path-name (gethash path-name *routing-table-x*))))



(defun find-alist-value-2 (key alist)
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

(defun path-to-x (name &rest args &key params)
  (x:if-let (route (gethash name *routing-table-x*))
    (let ((all-args (or params (x:plist-alist args))))
      
      (with-slots (arg-count original-string path-chunks) route
        (with-output-to-string (output)
          (if (zerop arg-count)
              (princ original-string output)
              
              (loop for chunk in path-chunks ;; (this coule be more FP)
                    do (if (stringp chunk)
                           (princ chunk output)

                           (progn
                             (multiple-value-bind (value found) (find-alist-value-2 chunk all-args)
                               (when found
                                 (princ value output)
                                 (setf all-args (remove-if #'(lambda (cell-key) (eq cell-key chunk))
                                                           all-args
                                                           :key #'car))))))))

          (when all-args
            (princ "?" output)
            (princ (quri:url-encode-params (mapcar #'(lambda (cell) (cons (string-downcase (string (car cell)))
                                                                          (cdr cell)))
                                                   all-args))
                   output)))))))
  











#|
  (defun chunk-path (path-string)
  (let ((path-segments (rest (cl-ppcre:split "\/" path-string))))
    
    (labels ((string-or-keyword (input)
               (if (and (> (length input) 1)
                        (starts-with-colon? input))
                   (x:make-keyword (string-upcase (subseq input 1)))
                   input))
             
             (generate-segment (segment next-bit previous-keyword?)
               (if (keywordp segment)
                   (cons segment next-bit)
                   
                   (if (stringp (car next-bit))
                       (cons (concatenate 'string segment (car next-bit))
                             (cdr next-bit))
                       
                       (cons segment
                             next-bit))))
             
             (consume-segment (path-segments previous-keyword?)
               
               (if path-segments
                   
                   (let ((segment (string-or-keyword (first path-segments)))
                         (next-bit (consume-segment (rest path-segments) (keywordp segment)))
                     
                     (if (keywordp segment)
                         (cons segment next-bit)

                         (if (stringp (car next-bit))
                             (cons (concatenate 'string segment (car next-bit))
                                   (cdr next-bit))
                             
                             (cons segment
                                   next-bit)))))))
                             
             (consume-segment path-segments nil))))











;; this one *almost works*
(defun chunk-path (path-string)
  (let ((path-segments (rest (cl-ppcre:split "\/" path-string))))
    
    (labels ((string-or-keyword (input)
               (if (and (> (length input) 1)
                        (starts-with-colon? input))
                   (x:make-keyword (string-upcase (subseq input 1)))
                   input))
             
             (generate-segment (segment the-rest previous-keyword?)

               (cons ;; segment
                     (if (keywordp segment)
                         segment
                         (if (stringp (first the-rest))
                             (concatenate 'string
                                          (if previous-keyword? "/" "")
                                          segment
                                          "/"
                                          (car the-rest))
                             (concatenate 'string
                                          (if previous-keyword? "/" "")
                                          segment)))
                             
                     (if (and (stringp segment)
                              (stringp (first the-rest)))
                         (cdr the-rest)
                         the-rest)))
                         
             
             (consume-segment (path-segments previous-keyword?)
               
               (if path-segments
                   (let ((segment (first path-segments)))
                     (generate-segment (string-or-keyword segment)
                                       (consume-segment (rest path-segments)
                                                        (keywordp segment))
                                     previous-keyword?) ))))
                             
      (consume-segment path-segments nil)))) 









  


(defun chunk-path (path-string)
  (let ((path-segments (cl-ppcre:split "\/" path-string)))
    (labels ((consume-segment (path-segments)
               (if path-segments
                   (cons (let ((segment (first path-segments)))
                           (if (starts-with-colon? segment)
                               (x:make-keyword (string-upcase (subseq segment 1)))
                               segment))
                         (consume-segment (rest path-segments))))
               ))
      (consume-segment path-segments))))

    (loop for segment in path-segments
          with arg-count = 0

          if (starts-with-colon? segment)
          collect "~a" into segment-parts
          and collect (x:make-keyword (string-upcase (subseq segment 1))) into segment-names
          and do (incf arg-count)
          else
          collect segment into segment-parts

	        finally (return (values (if segment-parts
				                              (format nil "~{~A~^/~}" segment-parts)
				                              "/")
			                            arg-count
                                  segment-names))))
  path-segments))













    (defun chunk-path (path-string)
  (let ((path-segments (cl-ppcre:split "\/" path-string)))
    
    (labels ((string-or-keyword (input)
               (if (and (> (length input) 1)
                        (starts-with-colon? input))
                   (x:make-keyword (string-upcase (subseq input 1)))
                   input))
             
             (generate-segment (segment the-rest previous-keyword?)

               (if (keywordp segment)
                   (cons segment the-rest)

                   (cons (if (stringp (first the-rest))
                             (concatenate 'string
                                          (if previous-keyword? "/" "")
                                          segment
                                          "/"
                                          (car the-rest))
                             
                             (concatenate 'string
                                          (if previous-keyword? "/" "")
                                          segment))
                   
                         (if (stringp (first the-rest))
                             (cdr the-rest)
                             the-rest))))
                         
             
             (consume-segment (path-segments previous-keyword?)
               
               (if path-segments
                   (let ((segment (string-or-keyword (first path-segments))))
                     (generate-segment segment
                                       (consume-segment (rest path-segments)
                                                        (keywordp segment))
                                     previous-keyword?) ))))
                             
      (consume-segment path-segments nil)))) 
























;; internal
(defun starts-with-colon? (input-string)
  "Does input-string start with a :?"
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
  path-chunks
  format-string ;; needed?
  args-by-position
  arg-count)

(defparameter *de-route-table* nil)

(export 'reset-de-routing-table)
(defun reset-de-routing-table ()
  (setf *de-route-table* (make-hash-table)))

(defun find-arg-value (key value-list)
  
  (labels ((walk-value-list (value-list out-list)
             (when value-list
               (let ((value-key (first value-list)))
                 (if (eq value-key key)
                     (values (second value-list)
                             out-list
                             t)
                     (walk-value-list (cddr value-list) out-list))))))
    (walk-value-list value-list nil)))

(export 'route-path-to)
(defun route-path-to (name &rest args)
  (x:if-let (entry (gethash name *de-route-table*))
    (with-slots (original-string format-string args-by-position arg-count) entry
      
      (if (zerop arg-count)
          ;; TODO: append the rest the args as params
          original-string

          (labels ((find-args-by-position (arg-specs args found-args other-args)
                     (let ((current-arg (first arg-specs)))
                       (multiple-value-bind (value found) (find-arg-value current-arg args)
                         (if found
                             (let ((new-args (append found-args (cons value nil))))
                               
                     ))

            (find-args-by-position args-by-position args nil nil))))

;;          (let ((arg-values-by-position (mapcar #'(lambda (arg-name)
;;                                                    (getf args arg-name))
;;                                                args-by-position)))
            
;;            arg-values-by-position)))

	  (error "A route of name '~a' could not be found" name)))

(defun --------route-path-to (name &rest args)
  (x:if-let (entry (gethash name *de-route-table*))
	  (let ((route-arg-count (route-path-arg-count entry)))
      
      (if (zerop route-arg-count)
          (route-path-original-string entry)
          
          (let ((arg-count (length args)))
	          (if (eq arg-count
		                route-arg-count)

	              (apply #'format (append (list nil
					                                    (route-path-format-string entry))
				                                args))

	              ;; incorrect number of arguments
	              (error "Incorrect number of arguments for route (wanted ~d, got ~d)"
		                   route-arg-count arg-count)))))

	  (error "A route of name '~a' could not be found" name)))


(export 'append-params) ;; a bit cheap-n-dirty but it'll work
(defun append-params (route-string &rest params)
  (let ((param-string (quri:url-encode-params (mapcar #'(lambda (prop)
                                                          (cons (string-downcase (car prop))
                                                                (cdr prop)))
                                                      (x:plist-alist params)))))
    (if (> (length param-string) 0)
           (format nil "~a?~a" route-string param-string)
           route-string)))

(defun build-de-route-format-string (path-segments)

  (loop for segment in path-segments
        with arg-count = 0

        if (starts-with-colon? segment)
        collect "~a" into segment-parts
        and collect (x:make-keyword (string-upcase (subseq segment 1))) into segment-names
        and do (incf arg-count)
        else
        collect segment into segment-parts

	      finally (return (values (if segment-parts
				                            (format nil "~{~A~^/~}" segment-parts)
				                            "/")
			                          arg-count
                                segment-names))))

(defun add-de-routing-point (name path-string path-segments)
  (if (gethash name *de-route-table*)
      (error "Route with name '~a' already defined" name))

  (multiple-value-bind (format-string arg-count arg-names) (build-de-route-format-string path-segments)
    (setf (gethash name *de-route-table*)
	        (make-route-path :original-string path-string
			                     :format-string format-string
			                     :arg-count arg-count
                           :args-by-position arg-names))))

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







TODO: try breaking the URL up into blocks like
URL /books/:bood-id/chapter/:chapter-id/paragraph/:paragraph-id
-> ("books" :BOOK-ID "chapter" :CHAPTER-ID "paragraph" :PARAGRAPH-ID)

then when call route-path-to
1. open an output string stream
2. iterate over the entry chunks
3.   write the plain segments out as normal
4.   lookup all the KEYWORD segment in the param args

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                ;;
;;   i am going to focus on rails for a sec and maybe get a job   ;;
;;                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *routing-table-x* (make-hash-table))

(defstruct route-path-x
  original-string
  path-chunks
  arg-count)

(defun chunk-path (path-string)
  (let ((path-segments (cl-ppcre:split "\/" path-string)))
    
    (labels ((string-or-keyword (input)
               (if (and (> (length input) 1)
                        (starts-with-colon? input))
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
                             
      (consume-segment path-segments nil))))

(defun build-route-path (path)
  (let ((chunks (chunk-path path)))
    (make-route-path-x :original-string path
                       :path-chunks chunks
                       :arg-count (count-if #'keywordp chunks))))

(defun register-route (name path)
  (setf (gethash name *routing-table-x*)
        (build-route-path path)))


(defun print-routes-x ()
  (loop for path-name being the hash-keys of *routing-table-x*
        do (format t "[~s] ~s~%" path-name (gethash path-name *routing-table-x*))))



(defun find-alist-value-2 (key alist)
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

(defun path-to-x (name &rest args &key params)
  (x:if-let (route (gethash name *routing-table-x*))
    (let ((all-args (or params (x:plist-alist args))))
      
      (with-slots (arg-count original-string path-chunks) route
        (with-output-to-string (output)
          (if (zerop arg-count)
              (princ original-string output)
              
              (loop for chunk in path-chunks ;; (this coule be more FP)
                    do (if (stringp chunk)
                           (princ chunk output)

                           (progn
                             (multiple-value-bind (value found) (find-alist-value-2 chunk all-args)
                               (when found
                                 (princ value output)
                                 (setf all-args (remove-if #'(lambda (cell-key) (eq cell-key chunk))
                                                           all-args
                                                           :key #'car))))))))

          (when all-args
            (princ "?" output)
            (princ (quri:url-encode-params (mapcar #'(lambda (cell) (cons (string-downcase (string (car cell)))
                                                                          (cdr cell)))
                                                   all-args))
                   output)))))))
  



|#
