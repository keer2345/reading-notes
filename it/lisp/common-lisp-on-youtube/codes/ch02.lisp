(defpackage rock-paper-scissors
  (:use :cl))
(in-package :rock-paper-scissors)

(defun get-player-choice (options)
  (format t "Please enter either of these -> [ ~{~A~^, ~} ]:" options)
  (force-output)

  (let ((choice (string-downcase (read-line))))
    (if (member choice options :test #'equal)
        choice
        (get-player-choice options))))

(defun game ()
  (let* ((options '("rock" "paper" "scissors"))
         (cpu-choice
           (nth (random (length options) (make-random-state t)) options))
         (player-choice (get-player-choice options)))


    (cond
      ; When a draw has occured
      ((equal cpu-choice player-choice)
       (format t "You entered: ~A, CPU entered: ~A. It's a Draw!~%" player-choice cpu-choice))

      ; If user enter rock and cpu enter scissors
      ((and (equal player-choice "rock")
            (equal cpu-choice "scissors"))
       (format t "You entered: ~A, CPU entered: ~A. You Win!~%" player-choice cpu-choice))

      ; If user enter paper and cpu enter rock
      ((and (equal player-choice "paper")
            (equal cpu-choice "rock"))
       (format t "You entered: ~A, CPU entered: ~A. You Win!~%" player-choice cpu-choice))

      ; If user enter scissors and cpu enter paper
      ((and (equal player-choice "scissors")
            (equal cpu-choice "paper"))
       (format t "You entered: ~A, CPU entered: ~A. You Win!~%" player-choice cpu-choice))

       (t (format t "You entered: ~A, CPU entered: ~A. You loose!~%" player-choice cpu-choice)))))

(game)