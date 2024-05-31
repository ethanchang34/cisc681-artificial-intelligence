(in-package :cl-user)
(defpackage :n-puzzle
  (:use :cl
        :common-lisp-user
        :alexandria)
  (:export :*puzzle-0*
   :*puzzle-1*
           :*puzzle-2*
   :*puzzle-3*
           :*puzzle-4*
   :*puzzle-5*
           :*puzzle-6*
   :*15-puzzle-1*
           :15-puzzle-2*
   :*goal-state*
   :*15-goal-state*
   :possible-actions
           :result
   :expand
           :iter-deep-search
   :breadth-first-search
           :a-star-search
   :num-misplaced-tiles
   :manhattan-distance))
(in-package :n-puzzle)

(defmacro dowhile (condition &body body)
  `(do ()
       ((not ,condition))
     ,@body))
(defmacro dountil (condition &body body)
  `(do ()
       (,condition)
     ,@body))

(defvar *goal-state* #2A((nil 1 2) (3 4 5) (6 7 8)))
(defvar *15-goal-state* #2A((nil 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15)))
(defvar *puzzle-0* #2A((3 1 2) (7 nil 5) (4 6 8)))
(defvar *puzzle-1* #2A((7 2 4) (5 nil 6) (8 3 1)))
(defvar *puzzle-2* #2A((6 7 3) (1 5 2) (4 nil 8)))
(defvar *puzzle-3* #2A((nil 8 6) (4 1 3) (7 2 5)))
(defvar *puzzle-4* #2A((7 3 4) (2 5 1) (6 8 nil)))
(defvar *puzzle-5* #2A((1 3 8) (4 7 5) (6 nil 2)))
(defvar *puzzle-6* #2A((8 7 6) (5 4 3) (2 1 nil)))
(defvar *15-puzzle-1* #2a((12 7 11 15)
                          (10 6 4 nil)
                          (1 2 9 3)
                          (13 5 8 14)))
(defvar *15-puzzle-2* #2a((12 7 11 15)
                          (9 6 4 nil)
                          (1 2 10 3)
                          (13 5 8 14)))
(defvar *blank-x*) ; 0 is the top row
(defvar *blank-y*) ; 0 is the left row


"------------------------------------------------------------------------------"
" Helper Functions"
"------------------------------------------------------------------------------"
(defun find-blank (board)
  "Find the nil space and store it's coordinates into two global variables"
  (loop for i below (array-dimension board 0)
        do (loop for j below (array-dimension board 1)
                 do (let ((element (aref board i j)))
                      (when (null element)
                        (setq *blank-x* i)
                        (setq *blank-y* j)
                        )))))

(defun my-copy-array (array)
  "Make a copy of an array. Used to ensure original states are unchanged"
  (make-array (array-dimensions array)
              :element-type (array-element-type array)
              :displaced-to (copy-seq (make-array (reduce #'* (array-dimensions array))
                                                  :element-type
                                                  (array-element-type array)
                                                  :displaced-to
                                                  array))))

(defun num-misplaced-tiles (board goal)
  "Return the number of misplaced tiles between a board state and the goal state"
  (let ((result 0))
    (dotimes (row (array-dimension board 0))
      (dotimes (col (array-dimension board 1))
        (when (not (eql (aref board row col) (aref goal row col)))
          (setq result (+ 1 result)))))
    result))

(defun manhattan-distance (board goal)
  "Return the Manhattan distance between a board state and the goal state"
  (cond ((and (eq (array-dimension board 0) (array-dimension goal 0))
              (eq (array-dimension board 1) (array-dimension goal 1)))
         (let ((h (make-hash-table))) ; key = tile value, value = (row col)
           (dotimes (r (array-dimension board 0))
             (dotimes (c (array-dimension board 1))
               (when (not (null (aref board r c)))
                 (setf (gethash (aref board r c) h) (list r c)))))
           (let ((sum 0))
             (dotimes (i (array-dimension goal 0))
               (dotimes (j (array-dimension goal 1))
                 (when (not (null (aref goal i j)))
                   (setq sum (+ sum (abs (- (car (gethash (aref goal i j) h)) i))))
                   (setq sum (+ sum (abs (- (cadr (gethash (aref goal i j) h)) j))))
                   )))
             sum)))
        (t (error "Input arrays must be the same dimension"))))

(defun flatten-2d-array (board)
  "Flatten a 2d array into a 1d array so inversion count is easier."
  (let ((result nil)
        (num-rows (array-dimension board 0))
        (num-cols (array-dimension board 1)))
    (dotimes (i num-rows)
      (dotimes (j num-cols)
        (let ((element (aref board i j))) ; Access element at i, j
          (when (not (null element)) ; Skip NIL elements
            (push element result)))))
    (reverse result)))

(defun inversion-count (board)
  "Return the inversion count of a board"
  (let ((count 0)
        (arr (flatten-2d-array board)))
    (loop for i from 0 to (- (length arr) 1)
          do (loop for j from i to (- (length arr) 1)
                do (when (> (nth i arr) (nth j arr))
                     (setq count (+ 1 count)))))
    count))

(defun solvablep (board)
  "For an N x N board, if N is odd then it's solvable if the inversion count is even
   If N is even, it's solvable if:
   the nil space is on an even row AND the inversion count is even
   OR the nil space is on an odd row AND the inversion count is odd
   ^ This is when the nil space in the goal state belong to the first top left tile"
  (let ((nil-row)
        (n (array-dimension board 0))
        (inv-count (inversion-count board)))
    (dotimes (i (array-dimension board 0))
      (dotimes (j (array-dimension board 1))
        (when (null (aref board i j))
          (setq nil-row i))))
    (if (oddp n)
        (evenp inv-count)
        (evenp (+ nil-row inv-count)))))
    
  
"------------------------------------------------------------------------------"
" Queue Functions (I had many issues with importing the package so I moved it here)"
"------------------------------------------------------------------------------"
(defun make-queue (&optional list)
  "If list is not nil, DESTRUCTIVELY make it into the new queue."
  (if list
      (cons list (last list))
      (list nil)))

(defun emptyp-q (queue)
  (not (car queue)))

(defun push-q (item queue)
  (if (cdr queue)
      (progn
        (setf (cddr queue) (list item))
        (setf (cdr queue) (cddr queue)))
      (progn
        (setf (cdr queue) (list item))
        (setf (car queue) (cdr queue))))
  queue)

(defun pop-q (queue)
  (prog1
      (pop (car queue))
    (when (emptyp-q queue)
      (setf (cdr queue) nil))))


"------------------------------------------------------------------------------"
" Priority Queue Functions (Moved functions here for the same reason above)"
"------------------------------------------------------------------------------"
(Defun make-pqueue (&key (predicate #'<) (key #'identity) (initial-size 4))
  "Returns an empty priority queue. Items in the queue will be
   ordered by PREDICATE on their keys. KEY is a function that takes an
   item and gives back the key of that item."
  (list :heap (make-array initial-size :fill-pointer 0)
        :predicate predicate
        :key key))

(defun emptyp-pq (queue)
  (zerop (fill-pointer (getf queue :heap))))

(defun parent (index)
  (if (oddp index) (/ (1- index) 2) (1- (/ index 2))))

(defun reheap-up (queue from)
  (do ((curr from (parent curr)))
      ((or (zerop curr)
           (not (funcall (getf queue :predicate)
                         (funcall (getf queue :key)
                                  (aref (getf queue :heap) curr))
                         (funcall (getf queue :key)
                                  (aref (getf queue :heap) (parent curr))))))
       queue)
    (rotatef (aref (getf queue :heap) (parent curr))
             (aref (getf queue :heap) curr))))

(defun push-pq (obj queue)
  (when (= (fill-pointer (getf queue :heap))
           (array-dimension (getf queue :heap) 0))
    (adjust-array (getf queue :heap)
                  (* 2 (fill-pointer (getf queue :heap)))))
  (incf (fill-pointer (getf queue :heap)))
  (setf (aref (getf queue :heap)
              (1- (fill-pointer (getf queue :heap))))
        obj)
  (reheap-up queue (1- (fill-pointer (getf queue :heap)))))

(defun pop-pq (queue)
  (labels ((smaller-child (index)
             (let* ((left-child (1+ (* 2 index)))
                    (right-child (1+ left-child)))
               (cond ((= left-child (1- (length (getf queue :heap)))) left-child)
                     ((< right-child (length (getf queue :heap)))
                      (if (funcall (getf queue :predicate)
                                   (funcall (getf queue :key)
                                            (aref (getf queue :heap) left-child))
                                   (funcall (getf queue :key)
                                            (aref (getf queue :heap) right-child)))
                          left-child
                          right-child))))))
    (decf (fill-pointer (getf queue :heap)))
    (rotatef (aref (getf queue :heap) 0)
             (aref (getf queue :heap)
                   (fill-pointer (getf queue :heap))))
    (do* ((smaller-child (smaller-child 0) (smaller-child smaller-child))
          (curr 0 (parent (or smaller-child 0))))
         ((or (null smaller-child)
              (not (funcall (getf queue :predicate)
                            (funcall (getf queue :key)
                                     (aref (getf queue :heap) smaller-child))
                            (funcall (getf queue :key)
                                     (aref (getf queue :heap) curr)))))
          (prog1 (aref (getf queue :heap) (fill-pointer (getf queue :heap)))
            (when (= (fill-pointer (getf queue :heap))
                     (/ (array-dimension (getf queue :heap) 0) 4))
              (adjust-array (getf queue :heap)
                            (* 2 (fill-pointer (getf queue :heap)))))))
      (rotatef (aref (getf queue :heap) smaller-child)
               (aref (getf queue :heap) curr)))))


"------------------------------------------------------------------------------"
"  Main Functions for Programming Assignment"
"------------------------------------------------------------------------------"
(defun possible-actions (board)
  "Lists possible actions for a given board state"
  (find-blank board)
  (let ((actions '() ))
    (when (not (= *blank-x* 0))
      (setq actions (cons :up actions)))
    (when (not (= *blank-x* (- (array-dimension board 0) 1)))
      (setq actions (cons :down actions)))
    (when (not (= *blank-y* 0))
      (setq actions (cons :left actions)))
    (when (not (= *blank-y* (- (array-dimension board 1) 1)))
      (setq actions (cons :right actions)))
    actions))

(defun result (board action)
  "Outputs the new board given a board and an action"
  (let ((new-board (my-copy-array board)))
    (case action
      (:up ; Swap blank space and above space
       (setf (aref new-board *blank-x* *blank-y*)
             (aref new-board (- *blank-x* 1) *blank-y*))
       (setf (aref new-board (- *blank-x* 1) *blank-y*)
             nil)
       (list action new-board))
      (:down ; Swap blank space and below space
       (setf (aref new-board *blank-x* *blank-y*)
             (aref new-board (+ *blank-x* 1) *blank-y*))
       (setf (aref new-board (+ *blank-x* 1) *blank-y*)
             nil)
       (list action new-board))
      (:left ; Swap blank space and left space
       (setf (aref new-board *blank-x* *blank-y*)
             (aref new-board *blank-x* (- *blank-y* 1)))
       (setf (aref new-board *blank-x* (- *blank-y* 1))
             nil)
       (list action new-board))
      (:right ; Swap blank space and right space
       (setf (aref new-board *blank-x* *blank-y*)
             (aref new-board *blank-x* (+ *blank-y* 1)))
       (setf (aref new-board *blank-x* (+ *blank-y* 1))
             nil)
       (list action new-board)))
    ))

(defun expand (board)
  "Outputs a list of board states that can be reached in one action"
  (let (next-board)
    (dolist (action (possible-actions board) next-board)
      (push (result board action) next-board))))

(defun depth-limited-search (board goal level)
  "Stack contains list of the form (board depth path)"
  "Outputs :cutoff if the solution could be deeper OR"
  "Outputs list of actions to reach goal state if it's found"
  (let ((stack ())
        (result :failure))
    (setq stack (cons (list board 0 ()) stack)) ; Push initial board state
    (dountil (null stack) ; While stack is not null
      (let ((node (car stack))) ; Node = top of stack
        ;(print node)
        (setq stack (cdr stack)) ; Pop stack
        (cond ((equalp (car node) goal) ; Check goal state
               (setq stack nil) ; Set stack to nil to get out of loop
               (setq result (reverse (caddr node)))) ; Set result to action path
              ((>= (cadr node) level) ; Check depth >= level (not > for my code)
               (setq result :cutoff))
              ; Check cycle if I have time to write...
              (t
               (dolist (child (expand (car node)))
                 (setq stack (cons (list (cadr child) (+ (cadr node) 1)
                                         (cons (car child) (caddr node))) stack)))))))
    result))

(defun iter-deep-search (board goal)
  "Iteratively increases the cut-off depth for depth-limited search"
  (if (solvablep board)
      (let ((result :cutoff)
            (level 0))
        (dowhile (eq result :cutoff)
          (setq result (depth-limited-search board goal level))
          (setq level (+ level 1)))
        result)
      (error "Unsolvable puzzle")))

(defun breadth-first-search (board goal)
  "BFS. FIFO queue contains lists of the form (board path)
   Outputs list of optimal actions to goal"
  (when (not (solvablep board))
    (error "Unsolvable puzzle"))
  (let ((visited (make-hash-table :test #'equalp))
        (q (make-queue))
        (result :failure))
    (push-q (list board ()) q) ; Initially push board and empty list of actions
    (if (equalp board goal) ; If initial board is goal state
        (setq result nil) ; Then no moves have been taken to reach goal
        (dountil (emptyp-q q) ; Else while queue is not null
          (let ((node (pop-q q))) ; Node = Pop FIFO queue
            (setf (gethash (car node) visited) 1) ; Add node to visited
            ;(print node)
            ;(print q)
            ;(print visited)
            (dolist (child (expand (car node))) ; Expand the node
              (when (equalp (cadr child) goal) ; Check if child is goal state
                (print "GOAL!")
                (setq result (reverse (cons (car child) (cadr node)))) ; Action path
                (setq q nil)
                (return))
              (when (not (gethash (cadr child) visited)) ; Check child in visited hash
                (push-q (list (cadr child) (cons (car child) (cadr node))) q)
                ; Push child to queue
                )) 
            )))
    result))

(defun a-star-search (board goal &optional (heuristic #'identity))
  "A* search. Priority queue contains lists of the form (board path cost)
   Outputs list of optimal actions to goal
   Heuristics: (constantly 0)  'num-misplaced-tiles  'manhattan-distance"
  (when (not (solvablep board))
    (error "Unsolvable puzzle"))
  (let ((visited (make-hash-table :test #'equalp))
        (pq (make-pqueue :key (lambda (%) (cadr %))))
        (result :failure))
    ; (initial board, f(n), action path) <- node format
    (push-pq (list board (funcall heuristic board goal) ()) pq) 
    (if (equalp board goal) ; If initial board is goal state
        (setq result nil) ; Then no moves have been taken to reach goal
        (block outside-loop
          (dountil (emptyp-pq pq) ; Else while queue is not null
            (let ((node (pop-pq pq))) ; Node = Pop priority queue
              (setf (gethash (car node) visited) 1) ; Add node to visited
              ;(print node)
              (dolist (child (expand (car node))) ; Expand the node
                ;(print child)
                (when (equalp (cadr child) goal) ; Check if child is goal state
                  (print "GOAL!")
                  (setq result (reverse (cons (car child) (caddr node)))) ; Action path
                  (return-from outside-loop))
                (when (not (gethash (cadr child) visited)) ; Check child in hash
                  ; Push child to priority queue
                  (push-pq (list (cadr child)
                                 (+ (funcall heuristic (cadr child) goal)
                                    (+ 1 (length (caddr node))))
                                 (cons (car child) (caddr node))) pq)
                  ))))))
    result))

