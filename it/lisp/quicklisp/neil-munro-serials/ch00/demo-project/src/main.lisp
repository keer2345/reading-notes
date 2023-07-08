(defpackage demo-project
  (:use :cl))
(in-package :demo-project)

;; blah blah blah.
(format t "Hello world!")

(defun hello-world (name)
  (format t "Hello, ~A!~%" name))

(hello-world "Neil")
