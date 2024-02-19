# [Rock Paper Scissors](https://www.youtube.com/watch?v=BRiS_enCbwA&list=PLCpux10P7KDKPb4eI5b_qSnQaY1ePGKGK&index=3&pp=iAQB)
## Warm up

### 列表的 `format`:
```lisp
;; options: '("rock" "paper" "scissors")

(format t "Please enter either -> ~{\"~A\"~^, ~}: " options)
;; Please enter either -> "rock", "paper", "scissors":

(format t "Please enter either -> ~{~A~^, ~}: " options)
;; Please enter either -> rock, paper, scissors:

```

### `member` 和它的 `:test`:
```lisp
(if (member (string-downcase (read-line)) options :test #'equal)   ;; 默认是 eq 方法，这里指定用 equal 方法
      (format t "It exists!~%")
      (format t "It does not exists!~%"))
```

### cond
```lisp
;; http://imangodoc.com/LMbMSTOU.html3

(cond
  (condition-1 expression-1 ...)
  (condition-2 expression-2 ...)
  ...
  (condition-n expression-n ...)
  (t expression-default ...))

```

## Code
`src/main.lisp`: 

```lisp
(defpackage rock-paper-scissors
  (:use :cl))
(in-package :rock-paper-scissors)

(defun get-player-choice (options)
  (format t "Please enter either -> ~{~A~^, ~}: " options)
  (force-output)

  (let ((choice (string-downcase (read-line))))
    (if (member choice options :test #'equal)
        choice
        (get-player-choice options))))

(defun game ()
  (let* ((options '("rock" "paper" "scissors"))
         (cpu-choice (nth (random (length options) (make-random-state t)) options))
         (player-choice (get-player-choice options)))

    (format t "You entered: ~A, CPU entered: ~A~%" player-choice cpu-choice)

    (cond
      ; When a draw has occured
      ((equal cpu-choice player-choice)
       (format t "Draw!~%"))

      ((or (and (equal player-choice "rock") (equal cpu-choice "scissors"))
           (and (equal player-choice "paper") (equal cpu-choice "rock"))
           (and (equal player-choice "scissors") (equal cpu-choice "paper")))
       (format t "You win!~%"))

      (t "You loose!~%"))))

(game)
```
