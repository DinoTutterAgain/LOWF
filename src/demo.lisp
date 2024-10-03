(defpackage :lowf.demo
  (:use :cl)
  (:local-nicknames (:html :lowf.html-view.tags))
  (:import-from :lowf.router
		:define-route-table
		:route)
  (:import-from :lowf.server
		:define-server-pre-start
		:set-public-directory))

(in-package :lowf.demo)

;;
;; app demo stuff (to be moved)
;;

(defmacro with-layout ((title) &body content)
  `(list
    (html:doctype "html")
    (html:html ()
      (html:head ()
	(html:title () ,title)
	(html:meta :property "og:site_name" :content "LOWF demo site")
	(html:meta :property "og:type" :content "website")
	(html:meta :property "og:description" :content "A sample website from build-site")
	(html:meta :charset "UTF-8"))
      (html:main ()
	,@content)
      (html:footer ()
	(html:p () "Copyright &copy; 2024, Me Corp")))))

(defun render-root ()
  (with-layout ("demo page")
    (html:h1 () "Hello!")))

(defun render-show ()
  (with-layout ("Show thing page")
    (html:h1 () "Showing a thing here")))

(defun act-on-index (request)
  (lowf.html-views:respond-html-view request (render-root)))

(defun act-on-show (request)
  (lowf.html-views:respond-html-view request (render-show)))

(define-route-table
  (route :get :root "/" #'act-on-index)
  (route :get :show "/image/:id" #'act-on-show))


(define-server-pre-start
  ;; called before the server is about to run
  (set-public-directory (merge-pathnames "public/"
					 (osicat:current-directory))))
