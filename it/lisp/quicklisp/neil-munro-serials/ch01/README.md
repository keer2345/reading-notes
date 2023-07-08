# Coin-Toss

- [Common Lisp Tutorial 1: Coin Toss](https://www.youtube.com/watch?v=3GEAINRCbJ4&list=PLCpux10P7KDKPb4eI5b_qSnQaY1ePGKGK&index=3)

## Create

``` shell
> rlwrap sbcl
* (cl-project:make-project #p"./coin-toss/" :author "keer2345" :email "keer2345@gmail.com" :license "LLGPL")
```

## Wram up
```lisp
(defpackage coin-toss
  (:use :cl))
(in-package :coin-toss)

(print "Hello world!")
(format t "Hello world!~%")
(format t "Hello ~A ~A!~%" "Neil" "Mnuro")

(defun hello-world (fname lname)
  (format t "Hello ~A ~A!~%" fname lname))

(hello-world "Neil" "Mnuro")
```
## Coin-toss code
```lisp
(defpackage coin-toss
  (:use :cl))
(in-package :coin-toss)

(defun toss-coin()
  "Generate a random heads or tails"
  (let ((number (random 2 (make-random-state t))))
    (if (= number 0)
        "heads"
        "tails")))

(defun prompt ()
  "Get user input and loop if it is not 'heads' or 'tails'"
  (format t "Please enter heads or tails: ")
  (force-output)

  (let ((guess (string-downcase(read-line))))
    (if (or (string= guess "heads") (string= guess "tails"))
        guess
        (prompt))))

(defun game ()
  "Run the actual game"
  (if (string= (prompt) (toss-coin))
      (format t "You Win!~%")
      (format t "You Loose!~%")))
```
