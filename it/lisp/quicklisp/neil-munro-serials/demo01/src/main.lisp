(defpackage demo01
  (:use :cl))
(in-package :demo01)

;; blah blah blah.
(format t "Hello world!~%")

(defun hello-world (name)
  (format t "Hello, ~A!~%" name))

(hello-world "Neil")

;; (ql:quickload :clsql)
