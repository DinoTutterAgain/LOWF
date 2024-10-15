(defpackage :lowf.demo
  (:use :cl)
  (:local-nicknames (:html :lowf.html-view.tags))
  (:import-from :lowf.router
		:define-route-table
		:route)
  (:import-from :lowf.server
		:define-server-pre-start
		:set-public-directory
		:define-not-found)
  (:import-from :lowf.html-views
		:define-layout
		:respond-html-view))

(in-package :lowf.demo)

;;
;; app demo stuff (to be moved)
;;
(define-layout (request contents)
  (declare (ignore request))
  (list
   (html:doctype "html")
   (html:html ()
     (html:head ()
       (html:title () "LOWF demo 2")
       (html:meta :property "og:site_name" :content "LOWF demo site")
       (html:meta :property "og:type" :content "website")
       (html:meta :property "og:description" :content "Demonstration of LOWF whilst I write LOWF")
       (html:meta :charset "UTF-8")
       (html:link :rel "stylesheet" :type "text/css" :href "styles.css"))

     (html:body ()
       (html:header ()
	 (html:a (:href "/" :id "logo") (html:span () "LOWF demo site"))
	 (html:nav ()
	   (html:a (:href "#") "Products")
	   (html:a (:href "#") "Services")
	   (html:a (:href "#") "Solutions")
	   (html:a (:href "#") "About")
	   (html:a (:href "#") "Log in")
	   (html:a (:href "#") "Sign up")))

       (html:main ()
	 contents)

       (html:footer ()
	 (html:p () "Copyright &copy; 2024, Me Corp"))))))

(defun render-root ()
  (html:h1 () "Hello!"))

(defun render-show ()
  (list
   (html:h1 () "Showing a thing here. toot")
   (html:p () (format nil "Captures: ~s" (hunchentoot:aux-request-value :path-captures)))))

(defun act-on-index (request)
  (respond-html-view request (render-root)))

(defun act-on-show (request)
  (respond-html-view request (render-show)))

(define-route-table
  (route :get :root "/" #'act-on-index)
  (route :get :show "/image/:id" #'act-on-show))

(define-not-found
  ;; (log-info "request=~s" hunchentoot:*request*)
  (lowf.html-views:respond-html-view
   nil
   (list
    (html:h1 () "404: Not found")
    (html:h2 () "The page you were looking for does not exist")
    (html:p () "Please check the URL and try again"))))

(define-server-pre-start
  ;; called before the server is about to run
  (set-public-directory (merge-pathnames "public/"
					 (osicat:current-directory))))
