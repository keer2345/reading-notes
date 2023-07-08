(defsystem "coin-toss"
  :version "0.1.0"
  :author "keer2345"
  :license "LLGPL"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "coin-toss/tests"))))

(defsystem "coin-toss/tests"
  :author "keer2345"
  :license "LLGPL"
  :depends-on ("coin-toss"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for coin-toss"
  :perform (test-op (op c) (symbol-call :rove :run c)))
