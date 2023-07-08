(defpackage demo-project/tests/main
  (:use :cl
        :demo-project
        :rove))
(in-package :demo-project/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :demo-project)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
