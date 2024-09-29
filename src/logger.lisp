(defpackage :lowf.logger
  (:use :cl))

(in-package :lowf.logger)

(defparameter *log-output-stream* t)

(defun write-log (time level line)
  (format *log-output-stream*
	  "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d ~a: ~a~%"
	  (local-time:timestamp-year   time)
	  (local-time:timestamp-month  time)
	  (local-time:timestamp-day    time)
	  (local-time:timestamp-hour   time)
	  (local-time:timestamp-minute time)
	  (local-time:timestamp-second time)
	  level
	  line)
  
  (finish-output *log-output-stream*))

(export 'log-info)
(defun log-info (format-string &rest args)
  (write-log (local-time:now)
	     :info
	     (apply #'format nil format-string args)))
    
(export 'log-warning)
(defun log-warning (format-string &rest args)
  (write-log (local-time:now)
	     :warning
	     (apply #'format nil format-string args)))

(export 'log-error)
(defun log-error (format-string &rest args)
  (write-log (local-time:now)
	     :warning
	     (apply #'format nil format-string args)))

