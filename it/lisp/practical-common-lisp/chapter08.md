# [8. Macros: Defining Your Own](https://gigamonkeys.com/book/macros-defining-your-own)

是时候开始写我们自己的宏了。

## Macro Expansion Time vs. Runtime
```lisp
(defun foo (x)
  (when (> x 10) (print 'big)))

(defmacro when (condition &rest body)
  `(if ,condition (progn ,@body)))
```

## DEFMACRO
## A Sample Macro: do-primes
分三步走。

首先，需要两个函数，一个测试指定的数字是否为质数，另一个返回下一个更大的质数。我们可以使用简单，但效率低下的方法：

```lisp
(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))
```
接下来编写宏。假设我们想要写出这样的样式：
```lisp
(do-primes (p 0 19)
  (format t "~d " p))
```
还没有宏的时候，我们使用 **DO** 循环来实现：
```lisp
(do ((p (next-prime 0) (next-prime (1+ p))))
    ((> p 19))
    (format t "~d " p))

; 2 3 5 7 11 13 17 19 
```
## Macro Parameters
