(defpackage demo01/tests/main
  (:use :cl
        :demo01
        :rove))
(in-package :demo01/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :demo01)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
