(asdf:defsystem "mcnibbles"
  :version "0.1.0"
  :author "Hamza Shahid"
  :license "BSD 2 Clause License"
  :depends-on ("mcclim")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "Nibbles clone in McCLIM"
  :in-order-to ((asdf:test-op (asdf:test-op "mcnibbles/tests"))))

(asdf:defsystem "mcnibbles/tests"
  :author "Hamza Shahid"
  :license "BSD 2 Clause License"
  :depends-on ("mcnibbles"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for mcnibbles"
  :perform (asdf:test-op (op c) (symbol-call :rove :run c)))
