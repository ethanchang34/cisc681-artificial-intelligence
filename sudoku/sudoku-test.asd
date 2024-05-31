(defsystem "sudoku-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Ethan Chang"
  :license ""
  :depends-on ("sudoku"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "sudoku"))))
  :description "Test system for sudoku"
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
