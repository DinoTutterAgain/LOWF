(defpackage :app.main
  (:use :cl)
  (:local-nicknames (:html :lowf.html-view.tags)
		                (:model :app.model)
		                (:x :alexandria))

  (:import-from :lowf.router
		            :define-route-table
		            :route-path-to
		            :pass-request-on
		            :define-wrapper)

  (:import-from :lowf.server
		            :define-server-pre-start)

  (:import-from :lowf.utils
		            :cassoc)

  (:import-from :lowf.logger
		            :log-info)

  (:import-from :lowf.html-views
		            :define-layout)

  (:import-from :lowf.request
		            :path-capture-value-integer
		            :www-form-params
                :request-path-params
		            :request-all-cookies
                :request-local-arg)

  (:import-from :lowf.response
		            :respond-html-view
		            :respond-redirect
		            :cookie
		            :respond-plaintext))

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

(define-layout (contents)
  (list
   (html:doctype "html")
   (html:html ()
     (html:head ()
       (html:title () "Mini TODO")
       (html:meta :property "og:site_name" :content "Mini TODO")
       (html:meta :property "og:type" :content "Demo application")
       (html:meta :property "og:description" :content "Demonstration of LOWF whilst I write LOWF")
       (html:meta :charset "UTF-8")
       (html:link :rel "stylesheet" :type "text/css" :href "/styles.css"))

     (html:body ()
       (html:header ()
	       (html:a (:href "/" :id "logo") (html:span () "Mini TODO&#9745;"))
	       (html:nav ()
	         (html:a (:href (route-path-to :root)) (html:span () "Home"))
	         (html:a (:href (route-path-to :new-item)) (html:span () "Add new"))
	         (html:a (:href (route-path-to :about)) (html:span () "About"))))

       (html:main ()
	       (html:div (:id "outer")
	         contents))

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
			                  (html:h3 () (html:a (:href (route-path-to :show-item :id (app.model:todo-item-id item))) (app.model:todo-item-name item)))
			                  (html:p () (format nil "Posted ~s" (app.model:todo-item-created-at item)))))
		              open-items))))))


;; TODO: the list of open TODOs

(defun render-about ()
  (let ((value (request-local-arg :local-value)))
    
    (list
     (html:h1 () "About Mini TODO")
     (html:p () "Just a small demo project walking you through how the functionality of LOWF would look in a more full-fledged application")
     (html:p () (format nil "local-value=~s" value))
     (html:p () "Some things it shows:")
     (html:ul ()
       (html:li () "one")
       (html:li () "two")
       (html:li () "three")
       (html:li () "four")
       (html:li () "five")))))

(defun render-show-item (item)
  (list
   (html:h1 () (model:todo-item-name item))
   (html:p () (format nil "(~d) Created ~s" (model:todo-item-id item) (model:todo-item-created-at item)))
   (when (> (length (or (model:todo-item-description item) ""))
	          0)
     (html:h2 () "Description")
     (html:div () (model:todo-item-description item)))))

(defun render-new-item ()
  (list
   (html:h1 () "New Item")
   (html:form (:action (route-path-to :create-item) :method "POST")
     (html:fieldset ()
       (html:label (:for "item-name") "Name")
       (html:input :type "text" :name "item-name" :value ""))

     (html:fieldset ()
       (html:label (:for "item-description") "Description")
       (html:textarea (:name "item-description" :rows 10)))

     (html:fieldset ()
       (html:button (:type "submit") "Create")))))

(defun render-route-not-found (message)
  (list
   (html:h1 () "404 Not Found")
   (html:p () (or message
		              "The thing you were looking for could not be found"))))

;;
;; controllers
;;

(defun respond-not-found (message)
  (respond-html-view (render-route-not-found message) :status 404))

(defun act-on-root ()
  (log-info "cookie=~s" (request-all-cookies))
  (log-info "path-params=~s" (request-path-params))
  (respond-html-view  (render-root)))

(defun act-on-about ()
  (setf (request-local-arg :local-value) 123)
  
  (respond-html-view (render-about)))

(defun act-on-show-item ()
  (x:if-let (item-id (path-capture-value-integer :id))
    (x:if-let (found-item (model:find-item item-id))
      (respond-html-view (render-show-item found-item))
      ;; else
      (respond-not-found "Couldn't find that item"))

    ;; else
    (respond-not-found "Bad or missing item ID")))

(defun act-on-new-item ()
  (respond-html-view (render-new-item)))

(defun act-do-create-item ()
  (let* ((parms (www-form-params))
	       (item-name-value (cassoc "item-name" parms t))
	       (item-description-value (cassoc "item-description" parms t)))

    ;;(respond-plaintext (format nil "post-params=~s" parms))))

    ;;  (lowf.request:with-post-parameters ((item-name "item-name"))
    (log-info "item-name-value=~s" item-name-value)
    (let ((new-item (app.model:add-item item-name-value item-description-value)))
      (log-info "new-item=~s" new-item)))

  (respond-redirect "/"))

(defun act-on-poke-cookie ()
  (respond-plaintext "Poking cookie" (list :set-cookie (cookie "UID" "4fe5a27f19cc0b864543"))))

;; (app.model:add-item "howdee")

(defun act-on-not-found ()
  (respond-not-found "Couldn't find that page you were looking for"))

;;
;; server stuff
;;

(define-wrapper (:demo-wrapper next method path)
  (log-info "Demo Wrapper")
  (let ((output (pass-request-on next method path)))
    output))

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

(define-server-pre-start
  )
