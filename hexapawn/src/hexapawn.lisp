; Author: Ethan Chang
(in-package :cl-user)
(defpackage :hexapawn
  (:use :cl
   :alexandria)
  (:export :*board*
   :to-move
           :actions
   :result
           :is-terminal
   :utility
           :minimax-search
   :minimax
           :sigmoid
   :relu
           :create-graph
   :classify
           :update-weights
   :train
   :train-hexapawn))
(in-package :hexapawn)

(defvar *board* #2A((:b :b :b) (nil nil nil) (:w :w :w)))
(defvar *policy-table* (make-hash-table :test #'equalp))
(defvar *nn*)
(defvar *activation*)
(defvar *activation-deriv*)
(defvar *predicted*)
(defvar *cache-in* ())
(defvar *cache-out* ())

"------------------------------------------------------------------------------"
"  Array, vector, list manipulation functions"
"------------------------------------------------------------------------------"
(defun puzzle-to-vector (array)
  "Takes the original 2D board and converts it in into a 1D vector
  Assumes the first player is white (0).
  #2A((:b :b :b) (nil nil nil) (:w :w :w) --> #(0 -1 -1 -1 0 0 0 1 1 1)"
  (let ((value)
        (flat-array (make-array (1+ (array-total-size array)))))
    (setf (aref flat-array 0) 0)
    (dotimes (i (array-dimension array 0))
      (dotimes (j (array-dimension array 1))
        (cond ((equal :B (aref array i j))
               (setq value -1))
              ((equal :W (aref array i j))
               (setq value 1))
              ((null (aref array i j))
               (setq value 0)))
        (setf (aref flat-array (+ (* i (array-dimension array 1)) j 1))
              value)))
    flat-array))

(defun array-to-vector (player-turn array)
  "Takes the player's turn (0 or 1) and a board that's a 2D array and converts 
  it into a 1D array (vector)
  NOT GENERALIZABLE"
  (let ((flat-array (make-array (1+ (array-total-size array)))))
    (setf (aref flat-array 0) player-turn)
    (dotimes (i (array-dimension array 0))
      (dotimes (j (array-dimension array 1))
        (setf (aref flat-array (+ (* i (array-dimension array 1)) j 1))
              (aref array i j))))
    flat-array))

(defun vector-to-array (array rows cols)
  "Takes a state represented by a vector and returns it as a 2D array.
  Player turn is omitted but should be kept in the memory by the parent function.
  For the scope of this project, rows and cols are both 3.
  (vector-to-array #(1 -1 -1 -1 0 0 0 1 1 1) 3 3) --> #2A((-1 -1 -1) (0 0 0) (1 1 1))
  NOT GENERALIZABLE"
  (let ((new-array (make-array (list rows cols))))
    (dotimes (i rows)
      (dotimes (j cols)
        (setf (aref new-array i j) (aref array (+ (* i cols) j 1)))))
    new-array))

(defun vec-to-arr (vector rows cols)
  "Generalizable vector to array"
  (let ((new-array (make-array (list rows cols))))
    (dotimes (i rows)
      (dotimes (j cols)
        (setf (aref new-array i j) (aref vector (+ (* i cols) j)))))
    new-array))

(defun arr-to-vec (array)
  "Generalizable array to vector"
  (let ((flat-array (make-array (array-total-size array))))
    (dotimes (i (array-dimension array 0))
      (dotimes (j (array-dimension array 1))
        (setf (aref flat-array (+ (* i (array-dimension array 1)) j))
              (aref array i j))))
    flat-array))

(defun array-to-list (array)
  "Converts a vector or array to a list"
  (let* ((dimensions (array-dimensions array))
         (depth      (1- (length dimensions)))
         (indices    (make-list (1+ depth) :initial-element 0)))
    (labels ((recurse (n)
               (loop for j below (nth n dimensions)
                     do (setf (nth n indices) j)
                     collect (if (= n depth)
                                 (apply #'aref array indices)
                                 (recurse (1+ n))))))
      (recurse 0))))

(defun list-to-array (lst rows cols)
  (if (= (* rows cols) (length lst))  ; Check if list length matches array dimensions
      (make-array `(,rows ,cols) :initial-contents lst)  ; Create 2D array with initial contents from list
      (error "List length does not match array dimensions.")))

(defun print-hash-table (hash-table)
  (loop for key being each hash-key in hash-table using (hash-value value) do
        (format t "~A => ~A~%" key value)))

(defun range (n)
  (loop for i from 0 to (- n 1) collect i))

(defun nshuffle (sequence)
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)

"------------------------------------------------------------------------------"
"  Main Functions for Programming Assignment"
"------------------------------------------------------------------------------"
(defun to-move (state)
  "Takes in a state (vector representation) and return's the player's turn for
  a given state. This is the first value in the vector."
  (aref state 0))

(defun actions (state)
  "Takes in a state and returns a list of legal actions.
  e.g., ((advance 2 0) (capture-left 2 2) (capture-right 2 1))
  Implementation converts the state to a 2D array for easier manipulation"
  (let ((turn (to-move state))
        (board (vector-to-array state 3 3))
        (moves))
    (let ((pawn)
          (forward))
      (if (equal turn 0)
          (progn
            (setq pawn 1) ; can represent enemy pawn by (* -1 pawn)
            (setq forward -1))
          (progn
            (setq pawn -1)
            (setq forward 1)))
      (dotimes (i (array-dimension board 0))
        (dotimes (j (array-dimension board 1))
          (when (equal pawn (aref board i j))
            ; if the pawn is not at the vertical edge (moved forward all the way)
            (when (and (>= (+ i forward) 0)
                       (<= (+ i forward) (- (array-dimension board 0) 1)))
              ; forward space is empty
              (when (equal (aref board (+ i forward) j) 0)
                (setq moves (cons (list :advance i j) moves)))
              ; not on left edge and enemy is diagonally left
              (when (and (> j 0)
                         (equal (aref board (+ i forward) (- j 1)) (* pawn -1)))
                (setq moves (cons (list :capture-left i j) moves)))
              ; not on right edge and enemy is diagonally right
              (when (and (< j (- (array-dimension board 1) 1))
                         (equal (aref board (+ i forward) (+ j 1)) (* pawn -1)))
                (setq moves (cons (list :capture-right i j) moves))))))))
    moves))

(defun result (state action)
  "Takes in a state and an action and returns the state resulting from
  taking the action on the given state"
  (when (null action)
    :error)
  (let ((turn (to-move state))
        (board (vector-to-array state 3 3))
        (move (car action))
        (i (cadr action))
        (j (caddr action))
        (forward))
    (if (equal turn 0)
        (setq forward -1)
        (setq forward 1))
    (case move
      (:advance
       (setf (aref board (+ i forward) j) (aref board i j)))
      (:capture-left
       (setf (aref board (+ i forward) (- j 1)) (aref board i j)))
      (:capture-right
       (setf (aref board (+ i forward) (+ j 1)) (aref board i j))))
    (setf (aref board i j) 0)
    (array-to-vector (logxor turn 1) board))) ; logxor will change the player turn

(defun is-terminal (state)
  "Returns a boolean indicating if the state is terminal.
  Terminal:
     A piece reaches the other side of the board.
     The opponent cannot make any moves. We don't need to check the own player's
       ability to move after making a move. Because, if you can't make any moves,
       it might not be terminal if the opponent has to make a move that frees one
       of your pieces.
  Thus, Assumes that is-terminal is called after making your move and setting
  the player turn to the opponent's."
  (let ((terminalp))
    (dotimes (i (length state))
      ; white piece reached the end of board
      (when (and (>= i 1) (<= i 3))
        (when (equal (aref state i) 1)
          (setq terminalp t)))
      ; black piece reached the end of board
      (when (and (>= i 7) (<= i 9))
        (when (equal (aref state i) -1)
          (setq terminalp t))))
    ; opponent cannot make any moves
    (when (null (actions state))
      (setq terminalp t))
    terminalp))

(defun utility (state)
  "Takes in a terminal state and returns the utility to player 1.
  The utility to player 2 is just the negation since this is a zero-sum game."
  (let ((turn (to-move state))
        (utility))
    (when (is-terminal state) ; sanity check that state is terminal
      (if (equal turn 0)
          (setq utility -1)
          (setq utility 1)))
    utility))

(defun minimax-search (board)
  "Takes in the initial *board* and builds a policy table.
  Calls minimax and uses a global hash table that stores the
  input state (key) if not already there, and stores the value as the
  max or min of its child states"
  (setf *policy-table* (make-hash-table :test #'equalp)) ; set hash to empty
  (let ((state (puzzle-to-vector board))
        (opt-value) ; optimal value
        (opt-action) ; optimal action
        (new-state) ; new state after taking an action
        (value)) ; some temp value for setting minimax calls to
    (let ((turn (to-move state)))
      (if (equal turn 0)
          (setf opt-value -2)
          (setf opt-value 2))
      (dolist (action (actions state))
        (setf new-state (result state action))
        (setf value (minimax new-state))
        (if (equal turn 0)
            (when (> value opt-value) ; player 0 = max
              (setf opt-value value)
              (setf opt-action (list action)))
            (when (< value opt-value) ; player 1 = min
              (setf opt-value value)
              (setf opt-action (list action))))
        (when (= value opt-value) ; multiple optimal actions
          (setf opt-action (append (list action) opt-action))))
      (setf (gethash state *policy-table*) (list opt-value opt-action))))
  *policy-table*
  ;(print-hash-table *policy-table*)
  )

(defun minimax (state)
  "Recursive minimax function for building a policy hash table."
  (if (is-terminal state) ; If state is terminal, return utility
      (utility state)
      (if (gethash state *policy-table*) ; if state already exists in table, return it
          (car (gethash state *policy-table*))
          (let ((opt-value 0)
                (opt-action)
                (new-state)
                (value)
                (turn (to-move state)))
            (if (equal turn 0)
                (setf opt-value -2)
                (setf opt-value 2))
            (dolist (action (actions state))
              (setf new-state (result state action))
              (setf value (minimax new-state))
              (if (equal turn 0)
                  (if (> value opt-value)
                      (progn
                        (setf opt-value value)
                        (setf opt-action (list action)))
                      (when (= value opt-value)
                        (setf opt-action (append (list action) opt-action))))
                  (if (< value opt-value)
                      (progn
                        (setf opt-value value)
                        (setf opt-action (list action)))
                      (when (= value opt-value)
                        (setf opt-action (append (list action) opt-action))))))
            (setf (gethash state *policy-table*) (cons opt-value (list opt-action)))
            opt-value))))

"------------------------------------------------------------------------------"
"  Math Functions"
"------------------------------------------------------------------------------"
(defun sigmoid (x)
  "Sigmoid function"
  (/ 1.0 (+ 1.0 (exp (- x)))))

(defun relu (x)
  "ReLU function"
  (max x 0))

(defun sigmoid-vec (vector)
  "Vectorized form of the sigmoid function"
  (map 'vector #'sigmoid vector))

(defun relu-vec (vector)
  "Vectorized form of the ReLU function"
  (map 'vector #'relu vector))

(defun sigmoid-deriv (x)
  "Derivative of sigmoid function"
  (* (sigmoid x) (- 1.0 (sigmoid x))))

(defun relu-deriv (x)
  "Derivative of ReLU function"
  (if (< x 0)
      0
      1))

(defun sigmoid-deriv-vec (vector)
  "Vectorized form of the sigmoid derivative function"
  (map 'vector #'sigmoid-deriv vector))

(defun relu-deriv-vec (vector)
  "Vectorized form of the ReLU derivative function"
  (map 'vector #'relu-deriv vector))

(defun subtract-matrices (matrix1 matrix2)
  "Function to subtract two matrices"
  (let ((result (make-array (array-dimensions matrix1))))
    (dotimes (i (array-dimension matrix1 0))
      (dotimes (j (array-dimension matrix1 1))
        (setf (aref result i j) (- (aref matrix1 i j) (aref matrix2 i j)))))
    result))

(defun add-vectors (vector1 vector2)
  "Function to add two vectors"
  (if (= (length vector1) (length vector2)) ; Check if vectors are of equal length
      (loop for i below (length vector1)
            collect (+ (aref vector1 i) (aref vector2 i))) ; Add corresponding elements
      (error "Vectors must be of equal length.")))

(defun subtract-vectors (vector1 vector2)
  "Function to subtract two vectors"
  (if (= (length vector1) (length vector2)) ; Check if vectors are of equal length
      (loop for i below (length vector1)
            collect (- (aref vector1 i) (aref vector2 i))) ; Subtract
      (error "Vectors must be of equal length.")))

(defun scalar-multiply-vector (scalar vector)
  "Scalar multiplication with a vector"
  (map 'vector (lambda (x) (* scalar x)) vector))

(defun scalar-multiply-matrix (scalar matrix)
  (let ((rows (array-dimension matrix 0))  ; Get number of rows
        (cols (array-dimension matrix 1))) ; Get number of columns
    (make-array `(,rows ,cols)
                :initial-contents
                (loop for i below rows
                      collect (loop for j below cols
                                    collect (* scalar (aref matrix i j)))))))

(defun element-wise-multiply (vector1 vector2)
  "Element-wise multiplication"
  (if (= (length vector1) (length vector2))  ; Check if vectors are of equal length
      (loop for i below (length vector1)
            collect (* (aref vector1 i) (aref vector2 i)))  ; Multiply corresponding elements
      (error "Vectors must be of equal length.")))

(defun transpose (matrix)
  "Transpose a matrix"
  (let ((rows (array-dimension matrix 0))  ; Get number of rows
        (cols (array-dimension matrix 1))) ; Get number of columns
    (make-array `(,cols ,rows)
                :initial-contents
                (loop for col below cols
                      collect (loop for row below rows
                                    collect (aref matrix row col))))))

(defun rand ()
  "Returns a random number between -1 and 1."
  (- (* (random 1.0) 2) 1))

"------------------------------------------------------------------------------"
"  Neural Network Functions"
"------------------------------------------------------------------------------"
(defun create-graph (&rest layer-sizes)
  "Creates a neural network (list of adjacency matrices and bias vectors)
  Args: list of layer sizes including input
    e.g. (create-graph 3 2 1) -->
         input of size 3, hidden layer of size 2, output of size 1"
  (let ((matrix-list)
        (bias-list))
    ; Create adjacency matrices
    (loop for i in layer-sizes
          for next-i in (cdr layer-sizes)
          do (setf matrix-list
                   (cons (make-array (list next-i i) :initial-element 1)
                         matrix-list)))
    ; Initialize weights to a random number between -1 and 1
    (loop for matrix in matrix-list
          do (dotimes (i (array-dimension matrix 0))
               (dotimes (j (array-dimension matrix 1))
                 (setf (aref matrix i j) (rand)))))
    ; Create bias vectors initialized to 1
    (loop for i in (cdr layer-sizes)
          do (setf bias-list (cons (make-array i :initial-element 1) bias-list)))
    (setf matrix-list (reverse matrix-list))
    (setf bias-list (reverse bias-list))
    (setf *nn* (list matrix-list bias-list))))

(defun classify (nn inputs activation)
  "Forward propagation of the neural network given the NN, a set of inputs
  matching the size of the input layer and an activation function
  (#'sigmoid-vec or #'relu-vec)"
  (let ((weight-matrices (car nn))
        (bias-vectors (cadr nn))
        (outputs inputs)
        (x))
    (setf *activation* activation)
    (if (equal *activation* #'relu-vec)
        (setf *activation-deriv* #'relu-deriv-vec)
        (setf *activation-deriv* #'sigmoid-deriv-vec))
    (setf *cache-in* ())
    (setf *cache-out* (list outputs))
    (loop for weights in weight-matrices
          for biases in bias-vectors
          do (setf x (coerce (add-vectors (lla:mm weights outputs) biases) 'vector))
             (setf outputs (funcall *activation* x))
             (setf *cache-in* (cons x *cache-in*))
             (setf *cache-out* (cons outputs *cache-out*)))
    (setf *cache-in* (reverse *cache-in*))
    (setf *cache-out* (reverse (cdr *cache-out*)))
    (setf *predicted* outputs)))

(defun update-weights (nn expected)
  "Takes an instance of the network and a vector of expected outputs and
  back propagates to modify the weights in the NN based on loss."
  (let ((err-deltas ())
        (in-i-list (reverse *cache-in*))
        (weights (reverse (car nn)))
        (biases (cadr nn))
        (new-weights ())
        (new-biases ())
        (lr 0.1))
    ; Generate error deltas into list of vectors
    (setf err-deltas (scalar-multiply-vector 2 (element-wise-multiply
                                         (coerce (subtract-vectors *predicted* expected) 'vector)
                                         (funcall *activation-deriv* (car in-i-list)))))
    (setf err-deltas (list err-deltas))
    (setf in-i-list (cdr in-i-list))
    (loop for in-i in in-i-list
          for w in weights
          do (setf err-deltas
                   (cons (coerce (element-wise-multiply
                                  (lla:mm (car err-deltas) w)
                                  (funcall *activation-deriv* in-i))
                                 'vector)
                         err-deltas)))
    ; Update weights and biases
    (setf weights (car nn))
    (loop for out-i in *cache-out*
          for w in weights
          for b in biases
          for err in err-deltas
          do (setf err (vec-to-arr err 1 (length err)))
             (setf out-i (vec-to-arr out-i 1 (length out-i)))
             (setf b (vec-to-arr b 1 (length b)))
             (setf new-weights (cons (subtract-matrices w (scalar-multiply-matrix lr (lla:mm (transpose err) out-i)))
                                     new-weights))
             (setf new-biases (cons (arr-to-vec (subtract-matrices b (scalar-multiply-matrix lr err))) new-biases)))
    (setf new-weights (reverse new-weights))
    (setf new-biases (reverse new-biases))
    ; Update NN to have new-weights and new-biases
    (setf (car *nn*) new-weights)
    (setf (cadr *nn*) new-biases)
    *nn*))

(defun train (inputs outputs)
  "Runs classify and update-weights for the given inputs and expected outputs
  inputs: #((0 0) (0 1) (1 0) (1 1))
  outputs: #((0 0) (0 1) (0 1) (1 0))"
  (if (not (equal (array-dimension inputs 0) (array-dimension outputs 0)))
      (error "Number of input samples must equal number of outputs")
      (let ((indices (range (array-dimension inputs 0))))
        (loop repeat 10000 ; Number of training cycles
              do (dolist (i (nshuffle indices))
                   (classify *nn* (coerce (aref inputs i) 'vector) #'sigmoid-vec)
                   (update-weights *nn* (coerce (aref outputs i) 'vector))))))
  *nn*)

(defun process-outputs (key value)
  "Outputs a vector of nine values representing optimal moves
  e.g., #(0 0 0 1 1 1 0 0 0)"
  (let ((turn (to-move key))
        (board (vector-to-array #(turn 0 0 0 0 0 0 0 0 0) 3 3)) ; dummy state
        (forward))
    (if (equal turn 0)
        (setq forward -1)
        (setq forward 1))
    (loop for action in value
          do (let ((move (car action))
                   (i (cadr action))
                   (j (caddr action)))
               (case move
                 (:advance
                  (setf (aref board (+ i forward) j) 1))
                 (:capture-left
                  (setf (aref board (+ i forward) (- j 1)) 1))
                 (:capture-right
                  (setf (aref board (+ i forward) (+ j 1)) 1)))))
    (subseq (array-to-vector (logxor turn 1) board) 1)))
        

(defun train-hexapawn ()
  (let ((inputs ())
        (outputs ()))
    (loop for key being the hash-keys of *policy-table*
          for value being the hash-values of *policy-table*
          do (setf inputs (cons (coerce key 'list) inputs))
             (setf outputs (cons (coerce (process-outputs key (cadr value)) 'list)
                                 outputs)))
    (train (coerce inputs 'vector) (coerce  outputs 'vector))))
