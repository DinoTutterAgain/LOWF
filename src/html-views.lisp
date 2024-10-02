(defpackage :lowf.html-views
  (:use :cl)
  (:local-nicknames (:html :lowf.html-view.tags)))

(in-package :lowf.html-views)

;; maybe don't overcomplicate this right now

(defparameter *closed-tags*
  '(:area
    :base
    :br
    :embed
    :hr
    :img
    :input
    :link
    :meta
    :param
    :source
    :track
    :wbr))

(defparameter *inline-nodes*
  '(:b
    :bdo
    :caption
    :cite
    :code
    :dd
    :del
    :dfn
    :em
    :figcaption
    :i
    :ins
    :kbd
    :label
    :legend
    :mark
    :meter
    :option
    :output
    :p
    :progress
    :q
    :rp
    :rt
    :s
    :small
    :span
    :strong
    :sub
    :sup
    :time
    :title
    :u
    :var))

(defun render-html (doc-tree)
  ;; bit of a chonker here
  (with-output-to-string (output)
    (labels ((consume-node (node inline indent)
	       
	       (labels ((node-indent (&optional inline-me)
			  (unless (or inline-me inline)
			    (format output "~a" indent)))
			
			(node-newline (&optional inline-me)
			  (unless (or inline-me inline)
			    (terpri output)))
			
			(node-write (obj)
			  (princ obj output))

			(node-format (pattern &rest arguments)
			  (apply #'format output pattern arguments))
			
			(process-args (args)
			  (when args
			    (let ((name (car args))
				  (value (cadr args)))
			
			      (when value
				(format output" ~a=~s" name value))
			      
			      (process-args (cddr args))))))

		 (cond
		   ;; is a bare string
		   ((stringp node)
		    (progn
		      (node-indent)
		      (node-write node)
		      (node-newline)))

		   ;; looks like a tag
		   ((keywordp (first node))
		    (let ((tag-name (first node)))

		      (block nil
			(node-indent)
			(node-write "<")

			(when (eq tag-name :doctype)
			  (node-format "!DOCTYPE ~a>" (second node))
			  (node-newline)
			  (return))

			(when (eq tag-name :comment)
			  (node-format "!-- ~a -->" (second node))
			  (node-newline)
			  (return))
			
			(node-write tag-name)

			(process-args (second node))

			(when (find tag-name *closed-tags*)
			  (node-write " />")
			  (node-newline)
			  (return))

			(node-write ">")

			;; content
			(let* ((children (cddr node))
			       (local-inline (or inline
						 (find tag-name *inline-nodes*)
						 (and (= (length children) 1)
						      (stringp (first children))))))

			  (when children
			    
			    (node-newline local-inline)

			    (let ((children-indent (concatenate 'string indent "  ")))
			      (loop for child in children
				    do (consume-node child
						     local-inline
						     children-indent))))
			  
			  ;; close out
			  (node-indent local-inline))
			
			(node-format "</~a>" tag-name)
			(node-newline))))

		    
		   ;; must be a list (?)
		   (t
		    (loop for sub-node in node
			  do (consume-node sub-node inline indent)))
			  
		   ))))

      (consume-node doc-tree nil ""))))


;;
;;
;;
;;
;;


(defparameter *layout-function* nil
  "the layout callback")

(defun default-layout (content-function)
  (html:html ()
    (funcall content-function)))

(setf *layout-function* #'default-layout)

;;(defparameter *views* (make-hash-table)
;;  "the page database")

;; (defmacro with-html-view (template)
;;   (lambda () ;; does the rendering
;;     ))



(export 'respond-html-view)
(defun respond-html-view (request doc-tree)
  ;; (maybe set the request/response mime type here?
  
  (render-html doc-tree))

;; (respond-html-fragment)

