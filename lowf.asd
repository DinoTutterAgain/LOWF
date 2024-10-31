
(asdf:defsystem #:lowf
  :description "A website"
  :author "Me."
  :version "0.0.1"
  :license "GPL"
  :serial t
  :depends-on (:postmodern ;; postgres access
	       
	       :ironclad ;; crypto
	       :cl-who ;; HTML rendering
	       :osicat ;; POSIX functions
	       :cl-ppcre ;; regex
	       :flexi-streams ;; streams
	       :quri ;; URL parsing
	       :hunchentoot ;; web server
	       :local-time ;; time calculations
	       :cl-base64 ;; storing big numbers as strings
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
				     (:file "server")))
				     ;;(:file "demo")))
	       
	       (:module "app"
			:components ((:file "model")
				     (:file "main")))))
	

			
