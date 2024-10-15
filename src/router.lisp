(defpackage :lowf.router
  (:use :cl)
  ;;(:import-from :ik.sys.lisp
  ;;		:make-keyword)
  (:import-from :lowf.logger
		:log-info))

(in-package :lowf.router)

;; (export 'make-keyword)
(defun make-keyword (name)
  "take a string, return a keyword"
  (intern (string-upcase name)
	  "KEYWORD"))

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

(defun build-de-route-format-string (path-segments)

  (loop for segment in path-segments
     with arg-count = 0
       
     if (starts-with-colon? segment)
     collect "~a" into segment-parts
     and do (incf arg-count)
     else
     collect segment into segment-parts
       
     finally (return (values (format nil "~{~A~^/~}" segment-parts)
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
	;;(format t "match!~%")
	(list t
	      (loop
		 for capture-value across captures
		 for name in (route-entry-match-names entry)
		 collect (cons (make-keyword name)
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
