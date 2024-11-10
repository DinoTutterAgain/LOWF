
(asdf:defsystem #:lowf
  :description "The Lisp Web Framework"
  :author "Me."
  :version "0.0.1"
  :license "GPL"
  :serial t
  :depends-on (;; :postmodern ;; postgres access
	       :alexandria ;; lisp extensions
	       :ironclad ;; crypto
	       ;; :cl-who ;; HTML rendering
	       :osicat ;; POSIX functions
	       :cl-ppcre ;; regex
	       :flexi-streams ;; streams
	       :quri ;; URL parsing
	       :woo ;; HTTP server
	       ;; :hunchentoot ;; web server
	       :local-time ;; time calculations
	       ;; :cl-base64 ;; storing big numbers as strings
	       )
  
  :components ((:module "src"
			:components ((:file "utils")
				     (:file "config")
				     (:file "logger")
				     (:file "router")
				     (:file "html-view-tags")
				     (:file "html-views")
				     (:file "request")
				     (:file "response")
				     (:file "static-files")
				     (:file "server")))
	       
	       (:module "app"
			:components ((:file "model")
				     (:file "main")))))
	

			
