(defpackage :lowf.router
  (:use :cl)
  (:import-from :alexandria
  		:make-keyword)
  (:import-from :lowf.logger
		:log-info))

(in-package :lowf.router)

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
;; (gethash :show-item *de-route-table*)

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
;; router
;;

(defstruct route-entry
  name
  method
  path-string

  pattern
  match-names

  callback)
  ;format
  ;format-args)

(defparameter *route-table* nil)

(export 'reset-route-table)
(defun reset-route-table ()
  (setf *route-table* nil))

;; helper
(defun build-route-entry (name method path path-segments callback)
  (multiple-value-bind (path-pattern pattern-names) (regexify-path path-segments)

    (make-route-entry :name name
		      :method method
		      :path-string path
		      :pattern path-pattern
		      :match-names pattern-names
		      :callback callback)))

;;(export 'add-route-point)
(defun add-routing-point (name method path path-segments callback)
  (setf *route-table*
	(append *route-table*
		(list (build-route-entry name method path path-segments callback)))))

;; internal
(defun match-for-path (entry method path)
  (when (eq method (route-entry-method entry))
    (multiple-value-bind (has-match captures) (cl-ppcre:scan-to-strings (route-entry-pattern entry) path)
      (when has-match
	(list t
	      (loop
		 for capture-value across captures
		 for name in (route-entry-match-names entry)
		 collect (cons (make-keyword (string-upcase name))
			       capture-value)))))))

(export 'route-request)
(defun route-request (method path)
  (loop for entry in *route-table*
     for (found matches) = (match-for-path entry method path)
     if found
     do (log-info "found ~a handler" (route-entry-name entry))
     and do (return (values (route-entry-callback entry)
			    matches))))


;;(export 'define-route-point)
(defun define-route-point (name method path-string callback)
  (let ((path-segments (cl-ppcre:split "\/" path-string)))
    (add-routing-point name method path-string path-segments callback)
    (add-de-routing-point name path-string path-segments)
    t))

(export 'route)
(defun route ())

(export 'define-route-table)
(defmacro define-route-table (&body body)
  (let ((route-name (intern "ROUTE" *package*)))
    `(labels ((,route-name (method name path callback)
		(define-route-point name method path callback)))
       (reset-route-table)
       (reset-de-routing-table)
       ,@body)))

(export 'print-route-table)
(defun print-route-table (&optional (stream t))
  (loop for route in *route-table*
	do (with-slots (name method path-string) route
	     (format stream "~a -> [~a] ~a~%" name method path-string))))


;; i am probably massively over-thinking this ;)
#|
so the idea is that all routing is done by walking through
a function tree and the first function that returns a woo response
is the point to stop and reply to the user request

try
- route
- route
- route
- wrapper
---- route
---- route
---- route
---- wrapper
------- route
etc


|#



(defun build-route-handler (method path-string function)
  (let* ((path-segments (cl-ppcre:split "\/" path-string))
	 (path-regex (regexify-path path-segments)))
	  
    (lambda (request-method request-path)
      (if (eq request-method method)
	  (multiple-value-bind (has-match captures) (cl-ppcre:scan-to-strings path-regex request-path)
	    (when has-match
	      ;; set captures
	      (funcall function)))))))

(defmacro try-routes ((method-var path-var) &body body)
  `(or ,@(mapcar #'(lambda (route-tester-method)
		     (list 'funcall route-tester-method
			   method-var path-var))
		 body)))

(defun try-routes-2 (method path &rest route-list)
  (find-if #'(lambda (v) (not (null v)))
	   route-list
	   :key #'(lambda (v) (funcall v method path))))

(try-routes-2 :get "/"
	      (build-route-handler :get "/" #'(lambda () (format t "root~%")))
	      (build-route-handler :get "/alpha" #'(lambda () (format t "alpha~%")))
	      (build-route-handler :get "/beta/:id" #'(lambda () (format t "beta with ID~%"))))

(defun poke-router (method path)
  (try-routes (method path)
    (build-route-handler :get "/" #'(lambda () (format t "root~%")))
    (build-route-handler :get "/alpha" #'(lambda () (format t "alpha~%")))
    (build-route-handler :get "/beta/:id" #'(lambda () (format t "beta with ID~%")))))


#|  
  (or (funcall (build-route-handler :get "/"
				    #'(lambda () (format t "root~%")))
	       method path)
      
      (funcall (build-route-handler :get "/alpha"
				    #'(lambda () (format t "alpha~%")))
	       method path)
      
      (funcall (build-route-handler :get "/beta"
				    #'(lambda () (format t "beta~%")))
	       method path)
      
      (funcall (build-route-handler :get "/beta/:id"
				    #'(lambda () (format t "beta with ID~%")))
	       method path)
      
      (funcall (build-route-handler :get "/cappa"
				    #'(lambda () (format t "cappa~%")))
	       method path)      
  ))
|#



#|
this is for use in a macro
(defun build-route-handler (method path-string function)
  (let* ((path-segments (cl-ppcre:split "\/" path-string))
	 (path-regex (regexify-path path-segments)))
	  
    `#'(lambda (request-method request-path)
	 (if (eq request-method ,method)
	     (multiple-value-bind (has-match captures) (cl-ppcre:scan-to-strings ,path-regex request-path)
	       (when has-match
		 ;; set captures
		 (funcall ,function)))))))
|#

(defmacro try-routes (&body body)
  `(or
    ,@body))

(defmacro do-get (path function)
  `(funcall #',(build-route-handler :get path function)))

(defparameter *wrapper-db* (make-hash-table))

(setf (gethash :flash *wrapper-db*)
      #'(lambda (next-function)
	  (format t "wrap: before~%")
	  (let ((result (funcall next-function)))
	    (format t "wrap after!~%")
	    result)))

(defmacro wrap (wrapper-name &body body)
  (let ((wrapper-function (gethash wrapper-name *wrapper-db*)))
    
    `(funcall #',wrapper-function
	      #'(lambda ()
		  ,@body))))

(TRY-ROUTES
  (do-get "/" 'handle-root)
  (do-get "/about" 'handle-about)
  (do-get "/login" 'handle-login)
  (do-post "/login" 'handle-do-login))



'(define-route-table
  (wrap :flash
   (try
    (get "/" 'handle-root)
    (post "/foo" 'handle-do-thing)

    (wrap :find-item
	  (wrap :find-user-from-cookie
		(wrap :user-must-be-logged-in
		      (try
		       (get "/item")))))

    (serve-static-files "")
    (invoke-not-found-handler))))


;;		  ,method ,path-regex ,function)))))




#|
    (multiple-value-bind (has-match captures) (cl-ppcre:scan-to-strings (route-entry-pattern entry) path)
      (when has-match
	(list t
	      (loop
		 for capture-value across captures
		 for name in (route-entry-match-names entry)
		 collect (cons (make-keyword (string-upcase name))
capture-value)))))))
|#






;;;;;;;;;;;;;;

;; i - need to wrap my head around macro expansions a bit here

(defun do-thing (marker)
  (format t "do-thing [~s]~%" marker)
  marker)

(defmacro macro-do-thing (&body body)
  (let ((value (do-thing (eval (cons 'list (mapcar #'(lambda (body-lambda) (list 'function body-lambda))
						   body))))))
    `(progn
       (do-thing :in-body)
       (format t "macro-do-thing: value=~s~%" ',value))))

;; (macro-do-thing
;;   (format t "macro-do-thing~%"))


(defun outer-method ()
  (macro-do-thing
    (build-route-handler :get "/" #'(lambda () (format t "root~%")))
    (build-route-handler :get "/alpha" #'(lambda () (format t "alpha~%")))
    (build-route-handler :get "/beta/:id" #'(lambda () (format t "beta with ID~%")))))


;;(defun outer-method ()
;;  (macro-do-thing
;;    (format t "in method macro-do-thing~%")))

(defun build-routing-table (route-spec)
  (labels ((process-get (args)
	     (let* ((try-path (first args))
		    (controller-method (second args))
		    (route-name (third args))
		   
		    (path-segments (cl-ppcre:split "\/" try-path))
		    (path-regex (regexify-path path-segments)))
	       
	       #'(lambda (method request-path)
		   (if (eq method :get)
		       (multiple-value-bind (has-match captures) (cl-ppcre:scan-to-strings path-regex request-path)
			 (when has-match
			   ;; set captures
			   (format t "GET ~s~%" try-path)
			   (funcall controller-method)))))))
			   ;; (format t "process-get: args=~s~%" args)))
	   
	   (process-post (args)
	     (let* ((try-path (first args))
		    (controller-method (second args))
		    (route-name (third args))
		    
		    (path-segments (cl-ppcre:split "\/" try-path))
		    (path-regex (regexify-path path-segments)))
	       
	       #'(lambda (method request-path)
		   (if (eq method :post)
		       (multiple-value-bind (has-match captures) (cl-ppcre:scan-to-strings path-regex request-path)
			 (when has-match
			   ;; set captures
			   (format t "POST ~s~%" try-path)
			   (funcall controller-method)))))))
	     ;; #'(lambda (method path) (format t "process-post: args=~s~%" args)))
	     
	   (process-wrap (args)
	     #'(lambda (method path) (format t "process-wrap: args=~s~%" args))))
    
    (mapcar #'(lambda (route-definition)
		(let ((type (first route-definition)))
		  (cond ((eq type :get)  (process-get (cdr route-definition)))
			((eq type :post) (process-post (cdr route-definition)))
			((eq type :wrap) (process-wrap (cdr route-definition)))
			(t (error "Unknown route verb ~s" type)))))
    
	    route-spec)))


(defun try-route-hack (method path)
  
  (labels ((fake-root-handler ()
	     (format t "fake-root-handler~%")
	     :fake-root-handler)
	   
	   (fake-about-handler ()
	     (format t "fake-about-handler~%")
	     :fake-about-handler)
    
	   (fake-login-handler ()
	     (format t "fake-login-handler~%")
	     :fake-login-handler))
    
    (let ((table (build-routing-table
		  (list (list :get "/" #'fake-root-handler :name)
			(list :get "/about" #'fake-about-handler)
			(list :get "/login" 'on-login :login)
			(list :post "/login" #'fake-login-handler)
			(list :wrap (list :wrapper-name)
			  (list :get "/thing-wrapped"))))))
      (labels ((poke-routes (method path)
		 (loop for try-route-entry in table
		       for result = (funcall try-route-entry method path)
		       if result return result)))
	
		 ;;(find-if #'(lambda (func) (and (funcall func method path)
		;;				t))
		;;	  table)))
	(poke-routes method path)))))

;; :get :post :any :not-found :static-files-from

