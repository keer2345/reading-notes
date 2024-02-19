# [Rock Paper Scissors](https://www.youtube.com/watch?v=BRiS_enCbwA&list=PLCpux10P7KDKPb4eI5b_qSnQaY1ePGKGK&index=3&pp=iAQB)
## Warm up

列表的 `format`:
```lisp
;; options: '("rock" "paper" "scissors")

(format t "Please enter either -> ~{\"~A\"~^, ~}: " options)
;; Please enter either -> "rock", "paper", "scissors":

(format t "Please enter either -> ~{~A~^, ~}: " options)
;; Please enter either -> rock, paper, scissors:

```

`member` 和它的 `:test`:
```lisp
(if (member (string-downcase (read-line)) options :test #'equal)   ;; 默认是 eq 方法，这里指定用 equal 方法
      (format t "It exists!~%")
      (format t "It does not exists!~%"))
```
