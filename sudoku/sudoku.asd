(defsystem "sudoku"
  :version "0.1.0"
  :author "Ethan Chang"
  :license ""
  :depends-on ("clack"
               "lack"
               "caveman2"
               "envy"
               "cl-ppcre"
               "uiop"
               "alexandria"

               ;; for @route annotation
               "cl-syntax-annot"

               ;; HTML Template
               "djula"

               ;; for DB
               "datafly"
               "sxql")
  :components ((:module "src"
                :components
                ((:file "queue")
                 (:file "sudoku-constraints")
                 (:file "sudoku")
                 (:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "config")
                 )))
  :description "Sudoku project for AI class"
  :in-order-to ((test-op (test-op "sudoku-test"))))
