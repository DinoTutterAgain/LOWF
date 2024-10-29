(defpackage :app.main
  (:use :cl)
  (:local-nicknames (:html :lowf.html-view.tags)
		    (:model :app.model))
  (:import-from :lowf.router
		:define-route-table
		:route
		:route-path-to)
  (:import-from :lowf.server
		:define-server-pre-start
		:set-public-directory)
  (:import-from :lowf.logger
		:log-info)
  (:import-from :lowf.html-views
		:define-layout)
  (:import-from :lowf.request
		:with-post-parameters
		:path-capture-integer)
  (:import-from :lowf.response
		:define-not-found
		:respond-html-view
		:respond-not-found-html))

(in-package :app.main)

;; a very simple todo app
;; - show open todos
;; - show all todos
;; - view one todo with extra info
;; - mark as done
;; - (unmark as done?)
;; - remove
;; - add new
;; - about

;;
;; views
;;

(define-layout (request contents)
  (declare (ignore request))
  (list
   (html:doctype "html")
   (html:html ()
     (html:head ()
       (html:title () "Mini TODO")
       (html:meta :property "og:site_name" :content "Mini TODO")
       (html:meta :property "og:type" :content "Demo application")
       (html:meta :property "og:description" :content "Demonstration of LOWF whilst I write LOWF")
       (html:meta :charset "UTF-8")
       (html:link :rel "stylesheet" :type "text/css" :href "todo.css"))

     (html:body ()
       (html:header ()
	 (html:a (:href "/" :id "logo") (html:span () "Mini TODO"))
	 (html:nav ()
	   (html:a (:href (route-path-to :root)) "Home")
	   (html:a (:href (route-path-to :new-item)) "Add new")
	   (html:a (:href (route-path-to :about)) "About")))

       (html:main ()
	 contents)

       (html:footer ()
	 (html:p () "Copyright &copy; 2024, Me Corp"))))))

(defun render-root ()
  (let ((open-items (app.model:open-items))
	(open-item-count (app.model:pending-item-count)))
    
    (list
     (html:h1 () "Mini TODO")
     (html:p () (html:a (:href (route-path-to :new-item)) "New item"))
     
     (if (zerop open-item-count)
	 (html:p () "CONGRATULATIONS! no open items exist!")
	 (list
	  (html:p () (format nil "Found ~d open items (~d total)" open-item-count (app.model:all-item-count)))
	  (mapcar #'(lambda (item)
		      
		      (html:div (:class "item")
			(html:h3 () (html:a (:href (route-path-to :show-item (app.model:todo-item-id item))) (app.model:todo-item-name item)))
			(html:p () (format nil "Posted ~s" (app.model:todo-item-created-at item)))))
		  open-items))))))


;; TODO: the list of open TODOs

(defun render-about ()
  (list
   (html:h1 () "About Mini TODO")
   (html:p () "Just a small demo project walking you through how the functionality of LOWF would look in a more full-fledged application")
   (html:p () "Some things it shows:")
   (html:ul ()
     (html:li () "one")
     (html:li () "two")
     (html:li () "three")
     (html:li () "four")
     (html:li () "five"))))

(defun render-show-item (item)
  (list
   (html:h1 () "Show item")
   (html:p () (format nil "item=~s" item))))

(defun render-new-item ()
  (list
   (html:h1 () "New Item")
   (html:form (:action (route-path-to :create-item) :method "POST")
     (html:fieldset ()
       (html:label (:for "item-name") "Name")
       (html:input :type "text" :name "item-name" :value ""))
     (html:fieldset ()
       (html:button (:type "submit") "Create")))))

(defun respond-not-found-html (message)
  (list
   (html:h1 () "404 Not Found")
   (html:p () (or message
		  "The thing you were looking for could not be found"))))

;;
;; controllers
;;

(defun act-on-root (request)
  (respond-html-view request (render-root)))

(defun act-on-about (request)
  (respond-html-view request (render-about)))

(defun act-on-show-item (request)
  (let ((id (path-capture-integer :id)))
    (if (numberp id)
	(let ((item (app.model:find-item id)))
	  (if item
	      (respond-html-view request (render-show-item item))
	      (respond-not-found-html "Could not find todo item with that ID")))
	(respond-not-found-html "Not a valid ID (must be an integer)"))))
	

(defun act-on-new-item (request)
  (respond-html-view request (render-new-item)))



  
(defun act-do-create-item (request)
  (format t "Howdee~%")
  (lowf.request:with-post-parameters ((item-name "item-name")) request
    (log-info "item-name=~s" item-name)
    (let ((new-item (app.model:add-item item-name)))
      (log-info "new-item=~s" new-item)))
  (hunchentoot:redirect "/"))

;;
;; server stuff
;;

(define-route-table
  (route :get :root "/" 'act-on-root)
  (route :get :about "/about" 'act-on-about)
  (route :get :new-item "/todo/new" 'act-on-new-item)
  (route :post :create-item "/todo/new" 'act-do-create-item)
  (route :get :show-item "/todo/:id" 'act-on-show-item))

(define-server-pre-start
  
  ;; called before the server is about to run
  (set-public-directory (merge-pathnames "app/public/"
					 (osicat:current-directory))))

