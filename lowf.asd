
(asdf:defsystem #:lowf
  :description "The Lisp Web Framework"
  :author "Me."
  :version "0.0.1"
  :license "GPL"
  :serial t
  :depends-on (;; :postmodern ;; postgres access
	             :alexandria ;; lisp extensions
	             :ironclad ;; crypto
	             :osicat ;; POSIX functions
	             :cl-ppcre ;; regex
	             :flexi-streams ;; streams
	             :quri ;; URL parsing
	             :woo ;; HTTP server
	             :local-time ;; time calculations
	             ;; :cl-base64 ;; storing big numbers as strings
	             )
  
  :components ((:module "src"
			                  :components ((:file "utils")
				                             (:file "config")
				                             (:file "logger")
				                             (:file "request")
				                             (:file "html-view-tags")
				                             (:file "html-views")
				                             (:file "response")
				                             (:file "router")
				                             (:file "server")))))


(asdf:defsystem #:lowf/demo
  :description "Simple demo application built upon LOWF"
  :author "Me."
  :version "0.0.1"
  :license "GPL"
  :serial t
  :depends-on (:lowf)
  :components ((:module "app"
			                  :components ((:file "model")
				                             (:file "main")))))
