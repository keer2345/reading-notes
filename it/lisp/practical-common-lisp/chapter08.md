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
