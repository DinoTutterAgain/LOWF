(defpackage :app.model
  (:use :cl)
  (:export :open-items
	   :pending-item-count
	   :all-items
	   :all-item-count
	   :add-item
	   :mark-item-done
	   :delete-item
	   :find-item
	   
	   :todo-item-id
	   :todo-item-name
	   :todo-item-description
	   :todo-item-created-at
	   :todo-item-completed-on
	   :todo-item-is-complete?))

(in-package :app.model)

(defstruct todo-item
  id
  name
  description
  created-at
  completed-on)

(defparameter *todo-items* nil)
;; (setf *todo-items* nil)

(let ((current-id-value 0))
  (defun next-id ()
    (incf current-id-value)))

;;
;; public API
;;

(defun todo-item-is-complete? (item)
  (not (null (todo-item-completed-on item))))

(defun open-items ()
  (remove-if #'todo-item-is-complete? *todo-items*))

(defun pending-item-count ()
  (count-if-not #'todo-item-is-complete? *todo-items*))

(defun all-items ()
  *todo-items*)

(defun all-item-count ()
  (length *todo-items*))

(defun add-item (name description)
  (push (make-todo-item :id (next-id)
			:name name
			:description description
			:created-at (local-time:now))
	*todo-items*))

(defun mark-item-done (id)
  (let ((item (find id *todo-items* :key #'todo-item-id :test #'eq)))
    (when item
      (format t "item=~s~%" item)
      (setf (todo-item-completed-on item)
	    (local-time:now)))))

(defun delete-item (id)
  (setf *todo-items*
	(remove-if #'(lambda (item) (eq (todo-item-id item)
					id))
		   *todo-items*)))

;; exported
(defun find-item (id)
  (find id *todo-items* :key #'todo-item-id :test #'=))

;; TODO: make items orderable
