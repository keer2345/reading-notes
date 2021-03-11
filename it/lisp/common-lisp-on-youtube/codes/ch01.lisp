(defpackage coin-toss
  (:use :cl))
(in-package :coin-toss)

; toss-coin function
(defun toss-coin ()
  "Generate a random heads or tails"
  (let ((number (random 2 (make-random-state t))))
    (if (zerop number)
        "heads"
        "tails")))

(defun prompt ()
  "Get user input and loop if it's not 'heads' or 'tails'"
  (format t "Please enter heads or tails: ")
  (force-output)

  (let ((guess (read-line)))
    (if (or (string= guess "heads")
            (string= guess "tails"))
        guess
        (prompt))))

(defun game ()
  "Run the actual game"
  (if (string= (prompt)
               (toss-coin))
      (format t "You win!~%")
      (format t "You Loose!~%")))