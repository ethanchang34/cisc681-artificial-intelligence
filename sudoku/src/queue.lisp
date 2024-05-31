(in-package :cl-user)
(defpackage :queue
  (:use :cl)
  (:shadow :push :pop)
  (:export :make-queue
           :emptyp
           :push
           :pop))

(in-package :queue)


(defun make-queue (&optional list)
  "If list is not nil, DESTRUCTIVELY make it into the new queue."
  (if list
      (cons list (last list))
      (list nil)))


(defun emptyp (queue)
  (not (car queue)))


(defun push (item queue)
  (if (cdr queue)
      (progn
        (setf (cddr queue) (list item))
        (setf (cdr queue) (cddr queue)))
      (progn
        (setf (cdr queue) (list item))
        (setf (car queue) (cdr queue))))
  queue)


(defun pop (queue)
  (prog1
      (cl:pop (car queue))
    (when (emptyp queue)
      (setf (cdr queue) nil))))
