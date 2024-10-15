(defpackage :app.main
  (:use :cl)
  (:local-nicknames (:html :lowf.html-view.tags)
		    (:model :app.model))
  (:import-from :lowf.router
		:define-route-table
		:route)
  (:import-from :lowf.server
		:define-server-pre-start
		:set-public-directory
		:define-not-found)
  (:import-from :lowf.logger
		:log-info)
  (:import-from :lowf.html-views
		:define-layout
		:respond-html-view))

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
	   (html:a (:href "#") "Home")
	   (html:a (:href "#") "Add new")
	   (html:a (:href "#") "About")))

       (html:main ()
	 contents)

       (html:footer ()
	 (html:p () "Copyright &copy; 2024, Me Corp"))))))

(defun render-root ()
  (html:h1 () "Mini TODO"))
;; TODO: the list of open TODOs

(defun render-about ()
  (html:h1 () "About Mini TODO"))

(defun render-show-item ()
  (html:h1 () "About Mini TODO"))


;;
;; controllers
;;

(defun act-on-root (request)
  (respond-html-view request (render-root)))

(defun act-on-about (request)
  (respond-html-view request (render-about)))

(defun act-on-show-item (request)
  (respond-html-view request (render-show-item)))

;;
;; server stuff
;;

(define-route-table
  (route :get :root "/" 'act-on-root)
  (route :get :about "/about" 'act-on-about)
  (route :get :show-item "/todo/:id" 'act-on-show-item))

(define-server-pre-start
  
  ;; called before the server is about to run
  (set-public-directory (merge-pathnames "app/public/"
					 (osicat:current-directory))))

