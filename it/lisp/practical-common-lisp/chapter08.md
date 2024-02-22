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
```lisp
(defmacro do-primes (var-and-range &rest body)
  (let ((var (first var-and-range))
        (start (second var-and-range))        
        (end (third var-and-range)))
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
       ,@body)))
```
运行：
```lisp
(do-primes (p 0 19)
           (format t "~d " p))
2 3 5 7 11 13 17 19 
NIL
```

进一步演化，可以使用列表 `(var start end)` 来替代 `var-and-range`，列表的三个元素可以自动结构成三个参数。
另外，可以使用 `&body` 来作为 `&rest` 的同义词，语义上是等价的，但很多开发环境使用 `&body` 参数以修改宏缩进的方式（通常 `&body` 参数
用于组成宏主体的表单列表）。
```lisp
(defmacro do-primes2 ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
     ,@body))
```
运行：
```lisp
(do-primes2 (p 0 19)
            (format t "~d " p))
2 3 5 7 11 13 17 19 
NIL
```
## Generating the Expansion

Backquote Syntax	|Equivalent List-Building Code|	Result
----|----|----
`(a (+ 1 2) c)	|(list 'a '(+ 1 2) 'c)	|(a (+ 1 2) c)
`(a ,(+ 1 2) c)	|(list 'a (+ 1 2) 'c)|	(a 3 c)
`(a (list 1 2) c)	|(list 'a '(list 1 2) 'c)	|(a (list 1 2) c)
`(a ,(list 1 2) c)	|(list 'a (list 1 2) 'c)	|(a (1 2) c)
`(a ,@(list 1 2) c)	|(append (list 'a) (list 1 2) (list 'c))	|(a 1 2 c)

**macroexpand-1**
```lisp
CL-USER> (macroexpand-1 '(do-primes (p 0 19) (format t "~d " p)))
(DO ((P (NEXT-PRIME 0) (NEXT-PRIME (1+ P))))
    ((> P 19))
  (FORMAT T "~d " P))
T
```

## Plugging the Leaks
