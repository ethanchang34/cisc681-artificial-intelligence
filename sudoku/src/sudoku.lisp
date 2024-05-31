; Author: Ethan Chang
(in-package :cl-user)
(defpackage :sudoku
  (:use :cl
   :alexandria)
  (:export :*4x4-puzzle*
   :*puzzle-1*
           :*puzzle-2*
   :*puzzle-3*
           :*puzzle-4*
   :*puzzle-5*
           :create-var-domains
   :backtracking-search
           :coordinates
           :create-csp
   :*4x4-constraints*
   :*9x9-constraints*))
(in-package :sudoku)

(defmacro dountil (condition &body body)
  `(do ()
       (,condition)
     ,@body))

(defvar *4x4-constraints* sudoku-constraints:*4x4-constraints*)
(defvar *9x9-constraints* sudoku-constraints:*9x9-constraints*)
(defvar *4x4-puzzle*
  '((1 nil nil nil)
    (nil 2 nil nil)
    (nil nil 3 nil)
    (nil nil nil 4)))
(defvar *puzzle-1*
  '((7 nil nil 4 nil nil nil 8 6)
    (nil 5 1 nil 8 nil 4 nil nil)
    (nil 4 nil 3 nil 7 nil 9 nil)
    (3 nil 9 nil nil 6 1 nil nil)
    (nil nil nil nil 2 nil nil nil nil)
    (nil nil 4 9 nil nil 7 nil 8)
    (nil 8 nil 1 nil 2 nil 6 nil)
    (nil nil 6 nil 5 nil 9 1 nil)
    (2 1 nil nil nil 3 nil nil 5)))
(defvar *puzzle-2*
  '((1 nil nil 2 nil 3 8 nil nil)
    (nil 8 2 nil 6 nil 1 nil nil)
    (7 nil nil nil nil 1 6 4 nil)
    (3 nil nil nil 9 5 nil 2 nil)
    (nil 7 nil nil nil nil nil 1 nil)
    (nil 9 nil 3 1 nil nil nil 6)
    (nil 5 3 6 nil nil nil nil 1)
    (nil nil 7 nil 2 nil 3 9 nil)
    (nil nil 4 1 nil 9 nil nil 5)))
(defvar *puzzle-3*
  '((1 nil nil 8 4 nil nil 5 nil)
    (5 nil nil 9 nil nil 8 nil 3)
    (7 nil nil nil 6 nil 1 nil nil)
    (nil 1 nil 5 nil 2 nil 3 nil)
    (nil 7 5 nil nil nil 2 6 nil)
    (nil 3 nil 6 nil 9 nil 4 nil)
    (nil nil 7 nil 5 nil nil nil 6)
    (4 nil 1 nil nil 6 nil nil 7)
    (nil 6 nil nil 9 4 nil nil 2)))
(defvar *puzzle-4*
  '((nil nil nil nil 9 nil nil 7 5)
    (nil nil 1 2 nil nil nil nil nil)
    (nil 7 nil nil nil nil 1 8 nil)
    (3 nil nil 6 nil nil 9 nil nil)
    (1 nil nil nil 5 nil nil nil 4)
    (nil nil 6 nil nil 2 nil nil 3)
    (nil 3 2 nil nil nil nil 4 nil)
    (nil nil nil nil nil 6 5 nil nil)
    (7 9 nil nil 1 nil nil nil nil)))
(defvar *puzzle-5*
  '((nil nil nil nil nil 6 nil 8 nil)
    (3 nil nil nil nil 2 7 nil nil)
    (7 nil 5 1 nil nil 6 nil nil)
    (nil nil 9 4 nil nil nil nil nil)
    (nil 8 nil nil 9 nil nil 2 nil)
    (nil nil nil nil nil 8 3 nil nil)
    (nil nil 4 nil nil 7 8 nil 5)
    (nil nil 2 8 nil nil nil nil 6)
    (nil 5 nil 9 nil nil nil nil nil)))


"------------------------------------------------------------------------------"
" Helper Functions"
"------------------------------------------------------------------------------"
(defun generate-list (n)
  "Creates a list from 1 to n like python's range() function"
  (let ((result nil))
    (dotimes (i n)
      (setf result (cons (+ 1 i) result)))
    (reverse result)))

(defun create-var-domains (puzzle)
  "Creates a list of variables and their domains given the puzzle"
  (let ((n (length (car puzzle)))
        (temp ()))
    (let ((range (generate-list n)))
      (loop for i from 1
            for row in puzzle
            do (loop for j from 1
                     for element in row
                     do (let ((key-var (intern (eval (list 'format nil "C~A~A" i j)) "KEYWORD")))
                          (if element
                              (setq temp (cons (cons key-var (list element)) temp))
                              (setq temp (cons (cons key-var range) temp)))))))
    (reverse temp)))

(defun create-csp (puzzle)
  "Appends the constraints to the variable domain"
  (let ((domains (create-var-domains puzzle))
        (n (length (car puzzle))))
    (cond ((equal n 4)
           (list (cons :VARIABLES domains) (cons :CONSTRAINTS *4x4-constraints*)))
          ((equal n 9)
           (list (cons :VARIABLES domains) (cons :CONSTRAINTS *9x9-constraints*))))))

(defun edit-csp (csp assignment)
  "Returns a new CSP with domains reflecting the new assignments"
  (let ((new-csp))
    (setq new-csp (copy-tree csp))
    (dolist (pair assignment)
      (setf (cdr (assoc (car pair) (cdr (assoc :VARIABLES new-csp)) :test #'equal))
            (cdr pair)))
    new-csp))

(defun initial-arcs (csp)
  "Create a list of the initial arcs to be put into the AC-3 queue"
  (let ((result)
        (constraints (cdr (assoc :CONSTRAINTS csp))))
    (loop for pair in constraints
          do (setq result (cons (car pair) result)))
    (reverse result)))

(defun valid-assignment (csp var value assignment)
  "Returns whether the new value tested for a variable is valid
  for the given assignment"
  (let ((constraints (cdr (assoc :CONSTRAINTS csp)))
        (valid t))
    (loop for i in assignment
          do (let ((constraint (or (cdr (assoc (list var (car i)) constraints
                                        :test #'equal))
                                   (cdr (assoc (list (car i) var) constraints
                                               :test #'equal)))))
               (when constraint
                 (when (not (member (list value (cadr i)) constraint :test #'equal))
                   (setq valid nil)
                   (return)))))
    valid))

"------------------------------------------------------------------------------"
"  Main Functions for Programming Assignment"
"------------------------------------------------------------------------------"
(defun revise (csp var1 var2)
  "Revises the CSP to remove inconsistent values based on constraints between
  two variables.
  Steps: Gets the domain for var1 and var2
         Gets the constraints for (var1 var2)
         Checks inconsistencies and destructively remove if so
  Args:
      csp: CSP of puzzle (e.g., ((:VARIABLES (:C11 1))
                                  :CONSTRAINTS (((:C11 :C12) (1 2) (2 1)))
      var1: The name of the first variable (e.g., :C11)
      var2: The name of the second variable (e.g., :C12)
  Returns:
      A boolean indicating whether any values were removed from var1's domain"
  (let ((removedp)
        (var1-domain (cdr (assoc var1 (cdr (assoc :VARIABLES csp)))))
        (var2-domain (cdr (assoc var2 (cdr (assoc :VARIABLES csp)))))
        (constraint (or (cdr (assoc (list var1 var2) (cdr (assoc :CONSTRAINTS csp))
                                    :test #'equal))
                        (cdr (assoc (list var2 var1) (cdr (assoc :CONSTRAINTS csp))
                                    :test #'equal)))))
    (let ((valid))
      ; Nested loop through var1 and var2 domains
      (dolist (x var1-domain)
        (dolist (y var2-domain)
          ; Check if at least one value in var2's domain satisfies the constraint
          (when (member (list x y) constraint :test #'equal)
            (setq valid t)))
        ; If no value in var2's domain satisfies the constraint, remove x in var1
        (when (not valid)
          (setf (cdr (assoc var1 (cdr (assoc :VARIABLES csp)))) (remove x var1-domain))
          (setq removedp t))
        (setq valid nil)))
    removedp))

(defun ac3 (csp)
  "Implements the AC-3 algorithm. Initializes a queue of arc binary constraints.
  Until the queue is empty, arcs are popped off the queue and checked for
  arc consistency.
  Follows the textbook pseudocode pretty closely.
  Args:
      csp: CSP of the puzzle
  Returns:
      A boolean indicating whether the csp remained arc consistent"
  (let ((init-arcs (initial-arcs csp))
        (domains (cdr (assoc :VARIABLES csp)))
        (consistent t))
    (let ((q (queue:make-queue init-arcs)))
      (dountil (queue:emptyp q)
        (let ((arc (queue:pop q)))
          (let ((x_i (car arc))
                (x_j (cadr arc)))
            (when (revise csp x_i x_j)
              (when (equal 0 (length (cdr (assoc x_i domains))))
                (setq consistent nil)
                (setq q nil)
                (return))
              (loop for pair in init-arcs
                    do (when (and (equal (cadr pair) x_i)
                                  (not (equal (car pair) x_j)) ; (car pair) == x_k
                                  (not (member pair (car q) :test #'equal)))
                         (queue:push (list (car pair) x_i) q)
                         )))))))
    consistent))

(defun minimum-remaining-values (csp assignment)
  "Heuristic that picks the unassigned variable with the smallest domain.
  Args:
      csp: CSP of the puzzle
      assignment: A list of variable assignments ((:C11 1) (:C12 2))
  Returns:
      A variable with the fewest values in its domain among the unassigned variables"
  (let ((domains (cdr (assoc :VARIABLES csp)))
        (assigned_vars (mapcar 'car assignment))
        (min-unassigned-var)
        (min-domain-length 999)) ; Initialized to some high number
    (loop for i in domains
          do (when (and (not (member (car i) assigned_vars :test #'equal))
                        (< (length (cdr i)) min-domain-length))
               (setq min-unassigned-var (car i))
               (setq min-domain-length (length (cdr i)))))
    min-unassigned-var))


(defun backtracking-search (puzzle)
  "Creates CSP from puzzle
  Creates list of initial assignments (from the puzzle)
  Calls backtrack"
  (let ((csp (create-csp puzzle))
        (init-assignments ()))
    (let ((domains (cdr (assoc :VARIABLES csp))))
      (loop for i in domains
            do (when (equal 1 (length (cdr i)))
                 (setq init-assignments (cons i init-assignments))))
      (setq init-assignments (reverse init-assignments))
      (ac3 csp)
      (backtrack csp init-assignments))))

(defun backtrack (csp assignment)
  "Recursive function that applies the minimum-remaining-values heuristic and AC-3
  Args:
      csp: CSP of the puzzle
      assignment: A list of variable assignments
  Returns:
      A complete and consistent assignment that solves the puzzle or :failure"
  (let ((domains (cdr (assoc :VARIABLES csp)))
        (var (minimum-remaining-values csp assignment)) ; Assign 'var' using heuristic
        (result :failure))
    ; If assignment is complete, return assignment
    ;(print var)
    ;(print domains)
    (if (equal (length assignment) (length domains))
        (setq result assignment)
        ; For each value in the domain of 'var'
        (dolist (value (cdr (assoc var domains)))
          ; Check is value is a valid assignment
          (when (valid-assignment csp var value assignment)
            ; Add value to assignment
            (if (assoc var assignment)
                (setf (cdr (assoc var assignment))
                           (cons value (cdr (assoc var assignment))))
                (setf assignment (append assignment (list (list var value)))))
            ;(print assignment)
            ; Create new CSP with new domains
            (let ((new-csp (edit-csp csp assignment)))
              ; Check arc consistency and limit domain search space
              (when (ac3 new-csp)
                ; Recurse
                (setq result (backtrack new-csp assignment))
                (when (not (equal result :failure))
                  (return)))))))
    result))

"------------------------------------------------------------------------------"
"  Helper Functions for Web Dev (Most are also defined in Web.lisp)"
"------------------------------------------------------------------------------"
(defun explodes (str position len)
   ;;; recursive helping function for explode
  (cond ((eq position len) nil)
        ((cons (read-from-string (string (schar str position)))
               (explodes str (1+ position) len)))))

(defun explode (atom)
   ;;; takes an atom and splits it into a list of single character
   ;;; atoms. Common lisp has no built-in explode function.
  (cond ((listp atom) atom)
        (t
         (let ((s (string atom)))
           (explodes s 0 (length s))))))

(defun coordinates (var)
  "Takes in a keyword var (:C11) and returns the coords (0 0)
  e.g., :C53 --> (4 2)"
  (mapcar #'1- (cdr (explode var))))

(defun init-assignments (puzzle)
  "Returns list of initial assignments"
  (let ((csp (create-csp puzzle))
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

(defun matrix-to-list (board)
  "Converts a #2A matrix to a list of lists"
  (loop for row below (array-dimension board 0)
        collect (loop for col below (array-dimension board 1)
                      collect (aref board row col))))

(defun make-board (assignment size)
  "Takes an assignment and board size and returns a #2A matrix"
  (let ((board (make-array (list size size)
                           :initial-element nil)))
    (dolist (var assignment board)
      (destructuring-bind (row col) (coordinates (car var))
        (setf (aref board row col) (copy-seq (cdr var)))))))

(defun make-boards (puzzle)
  "Returns list of boards in list format"
  (let ((solution (backtracking-search puzzle))
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
      
