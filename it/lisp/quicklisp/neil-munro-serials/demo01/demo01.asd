(defsystem "demo01"
  :version "0.1.0"
  :author "keer"
  :license "LLGPL"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "demo01/tests"))))

(defsystem "demo01/tests"
  :author "keer"
  :license "LLGPL"
  :depends-on ("demo01"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for demo01"
  :perform (test-op (op c) (symbol-call :rove :run c)))
