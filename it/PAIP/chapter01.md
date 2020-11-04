**Chapter 01 [Introduction to Lisp](https://github.com/norvig/paip-lisp/blob/master/docs/chapter1.md)**

本章主要是给没有 Lisp 经验的程序员阅读的。计算机的使用者和程序员主要区别是前者是输入，后者定义新的操作、程序、甚至是数据类型。

推荐的环境：Sbcl + Quicklisp + Emacs + Slime

# 符号计算
```lisp
(+ 2 3)

(append '(Pat Kim) '(Robin Sandy)) => (PAT KIM ROBIN SANDY)
```
```lisp
> (append '(Pat Kim) (list '(John Q Public) 'Sandy))
(PAT KIM (JOHN Q PUBLIC) SANDY)

> (length (append '(Pat Kim) (list '(John Q Public) 'Sandy)))
4
```

关于符号需要注意四点：

1. Lisp 并不会给它操作的对象赋予而外的含义。对 Lisp 来说它们都是符号。
1. Common Lisp 提供了 700 多个内置的函数，在某些时候，读者应该浏览参考文献，看看里面有什么，但是大多数重要的功能在本书的第一部分中都有介绍。
1. 在 Common Lisp 中符号不区分大小写的。
1. 符号允许多种字符组成：数字、字母、以及其他例如 `+`、`!` 的标点符号。

# 变量

赋予值给变量的一种方式是用 `setf`：
```lisp
> (setf p '(John Q Public)) => (JOHN Q PUBLIC)
> p => (JOHN Q PUBLIC)
> (setf x 10) => 10
> (+ x x) => 20
> (+ x (length p)) => 13
```
