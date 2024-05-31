(defsystem "hexapawn-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Ethan Chang"
  :license ""
  :depends-on ("hexapawn"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "hexapawn"))))
  :description "Test system for hexapawn"
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
