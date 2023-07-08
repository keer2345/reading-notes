(defsystem "demo-project"
  :version "0.1.0"
  :author "keer2345"
  :license "LLGPL"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "demo-project/tests"))))

(defsystem "demo-project/tests"
  :author "keer2345"
  :license "LLGPL"
  :depends-on ("demo-project"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for demo-project"
  :perform (test-op (op c) (symbol-call :rove :run c)))
