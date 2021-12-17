(asdf:defsystem "snakeronni"
  :version "0.1.0"
  :author "Hamza Shahid"
  :license "BSD 2 Clause License"
  :depends-on ("mcclim")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "A Modern Version of the Classical Snake Game"
  :in-order-to ((asdf:test-op (asdf:test-op "snakeronni/tests"))))

(asdf:defsystem "snakeronni/tests"
  :author "Hamza Shahid"
  :license "BSD 2 Clause License"
  :depends-on ("snakeronni"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for snakeronni"
  :perform (asdf:test-op (op c) (symbol-call :rove :run c)))
