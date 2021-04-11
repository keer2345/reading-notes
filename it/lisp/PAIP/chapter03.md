**[Chapter 03 Overview of Lisp](https://github.com/norvig/paip-lisp/blob/master/docs/chapter3.md)**

本章讲述几个特殊的 Lisp 函数，对有经验的 Common Lisp 程序员可以跳过，但对于 Lisp 新手或者刚入门 Common Lisp 方言的程序员来说是十分必要的。

本章只是可以作为参考，权威的参考书是 Steele 的《Common Lisp the Language, 2th edition》。

# 3.1 Lisp 风格指南

使用最具体的形式可以让你的读者更容易理解你的意图。例如，条件样式 `when` 比 `if` 更具体。

具体化的重要方式是使用抽象，Lisp 提供了通用的数据结构，例如 lists 和 arrays，它们可用于实现程序想要的特定的数据结构，但不应该犯直接调用原始函数的错误。如果定义一个名称列表：
```lisp
(defvar *names* '((Robert E. Lee) ...))
```
然后还应定义函数来获取名称的元素。获取 `Lee` 可以使用 `(last-name (first *names*))`，而不是 `(caddar *names*)`。

这些原则往往是一致的。例如，如果你的代码尝试在列表中找到一个元素，你可以使用 `find` 或者 `find-if`，而不是 `loop` 或 `do`。

有时候，原则也会有冲突的：
```lisp
(push (cons key val) a-list)
(setf a-list (acons key val a-list))
```

# 3.2 指定样式
definitions	|conditional	|variables	|iteration	|other
----|----|----|----|----
defun	|and	|let	|do	|declare
defstruct|case|let*|do*|function
defvar|cond|pop|dolist|progn
defparameter|if|push|dotimes|quote
defconstant|or|setf|loop|return
defmacro|unless|incf||trace
labels|when|decf||untrace

```lisp
(defun function-name (parameter...) "optional documentation" body...)

(defmacro macro-name (parameter...) "optional documentation" body...)

(defvar variable-name initial-value "optional documentation" )

(defparameter variable-name value "optional documentation")

(defconstant variable-name value "optional documentation")
```

大多数编程语言都提供了一种将相关数据组合成一个结构的方法，Lisp 也不例外。
```lisp
(defstruct structure-name "optional documentation" slot...)
```
例子如下：
```lisp
(defstruct name 
  first
  (middle nil)
  last)
```

这里自动地定义了构造函数 `make-name`，识别器 `name-p`，访问器 `name-first`, `name-middle` 和 `name-last`。
```lisp
> (setf b (make-name :first 'Barney :last 'Rubble)) =>
#S(NAME :FIRST BARNEY :LAST RUBBLE)

> (name-first b) => BARNEY

> (name-middle b) => NIL

> (name-last b) => RUBBLE

> (name-p b) => T

> (name-p 'Barney) => NIL ; only the results of make-name are names

> (setf (name-middle b) 'Q) => Q

> b => #S(NAME :FIRST BARNEY :MIDDLE Q :LAST RUBBLE)
```