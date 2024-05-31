(in-package :cl-user)
(defpackage sudoku.web
  (:use :cl
        :caveman2
        :sudoku.config
        :sudoku.view
        :sudoku.db
        :datafly
        :sxql)
  (:export :*web*))
(in-package :sudoku.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

(let ((sharpless-readtable (copy-readtable)))
  (set-macro-character #\#
                       #'(lambda (&rest _)
                           (declare (ignore _))
                           (error "Encountered '#' in input"))
                       nil
                       sharpless-readtable)
  (defun read-untrusted-from-string (istring)
    (let (*read-eval* (*readtable* sharpless-readtable))
      (read-from-string istring))))

(defun matrix-to-list (board)
  "Converts a #2A matrix to a list of lists"
  (loop for row below (array-dimension board 0)
        collect (loop for col below (array-dimension board 1)
                      collect (aref board row col))))

(defun init-assignments (puzzle)
  "Returns list of initial assignments"
  (let ((csp (sudoku:create-csp puzzle))
        (init-assignments ()))
    (let ((domains (cdr (assoc :VARIABLES csp))))
      (loop for i in domains
            do (when (equal 1 (length (cdr i)))
                 (setq init-assignments (cons i init-assignments))))
      (setq init-assignments (reverse init-assignments)))))

(defun new-assignments (puzzle solution)
  "Returns list of assignments for solution minus initial assignments"
  (let ((new-assigns solution)
        (inits (init-assignments puzzle)))
    (loop for i in inits
          do (setq new-assigns (remove i new-assigns :test #'equal)))
    new-assigns))

(defun make-board (assignment size)
  "Converts a #2A matrix to a list of lists"
  (let ((board (make-array (list size size)
                           :initial-element nil)))
    (dolist (var assignment board)
      (destructuring-bind (row col) (sudoku:coordinates (car var))
        (setf (aref board row col) (copy-seq (cdr var)))))))

(defun make-boards (puzzle)
  "Returns list of boards in list format"
  (let ((solution (sudoku:backtracking-search puzzle))
        (size (length (car puzzle)))
        (list-of-boards ()))
    (let ((inits (init-assignments puzzle))
          (new-assigns (new-assignments puzzle solution))
          (curr-assigns))
      (setq curr-assigns inits)
      (setq list-of-boards (list (matrix-to-list (make-board curr-assigns size))))
      (dolist (assign new-assigns)
        (setq curr-assigns (cons (list (car assign) (cadr assign)) curr-assigns))
        (setq list-of-boards (cons (matrix-to-list (make-board curr-assigns size)) list-of-boards))))
    (reverse list-of-boards)))

;;
;; Routing rules

(defroute "/" (&key (|puzzle| "NIL"))
  (render #P"index.html"
          (list :boards (make-boards (read-untrusted-from-string |puzzle|)))))

(defroute "/fourxfour" ()
  (render #P"index.html"
          (list :boards (make-boards sudoku:*4x4-puzzle*))))

;(defoute "/fancy" (&key (|puzzle| "NIL"))
;  (render #P"fancy.html"
;          (multiple-value-call #'make-boards++
;            (sudoku:backtracking-search++ (read-untrusted-from-string |puzzle|)))))


;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
