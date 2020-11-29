**Chapter 03 [Functions](http://lispcookbook.github.io/cl-cookbook/functions.html)**

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [命名函数](#命名函数)
- [参数](#参数)
    - [必要参数](#必要参数)
    - [可选参数](#可选参数)
    - [关键字参数](#关键字参数)
    - [默认值](#默认值)
    - [不定参数](#不定参数)
- [返回值](#返回值)
    - [返回多值](#返回多值)
        - [values](#values)
        - [multiple-value-bind](#multiple-value-bind)
        - [nth-value](#nth-value)
        - [multiple-value-list](#multiple-value-list)
        - [value-list](#value-list)
- [匿名函数lambda](#匿名函数lambda)
- [函数的调用](#函数的调用)
    - [函数的引用](#函数的引用)
- [高阶函数](#高阶函数)
- [闭包](#闭包)
- [函数setf](#函数setf)
- [柯里化Currying](#柯里化currying)
    - [概念](#概念)
    - [alexandria库](#alexandria库)
- [参考文档](#参考文档)

<!-- markdown-toc end -->

# 命名函数

`defun`

``` lisp
(defun <name> (list of arguments)
  "docstring"
  (function body))
```

# 参数
## 必要参数
```lisp
(defun hello (name)
  "Say hello to `name'."
  (format t "hello ~a !~&" name))
;; HELLO
```

`~a` 传入变量，`~&` 为换行。结果如下：
```
(hello "me")
;; hello me !  <-- this is printed by `format`
;; NIL         <-- return value: `format t` prints a string to standard output and returns nil.
```
## 可选参数
`&optional`

``` lisp
(defun hello (name &optional age gender) …)
```
可以这样调用：
``` lisp
(hello "me") ;; a value for the required argument, zero optional arguments
(hello "me" "7")  ;; a value for age
(hello "me" 7 :h) ;; a value for age and gender
```
## 关键字参数
`&key`

通常情况下难以记住入参的顺序，我们引入关键字参数 `&key <name>`，通过类似 `:name <value>` 的方式调用，如果没有找到 `name` 关键字，则默认为 `nil`：
``` lisp
(defun hello (name &key happy)
  "If `happy' is `t', print a smiley"
  (format t "hello ~a " name)
  (when happy
    (format t ":)~&"))
```
可以这样调用：
```lisp
(hello "me")
(hello "me" :happy t)
(hello "me" :happy nil) ;; useless, equivalent to (hello "me")
```
但是 `(hello "me" :happy)` 是非法的。

几个关键字入参的例子：
```lisp
(defun hello (name &key happy lisper cookbook-contributor-p) …)
```
调用 0 个或者多个关键字入参：
```lisp
(hello "me" :lisper t)
(hello "me" :lisper t :happy t)
(hello "me" :cookbook-contributor-p t :happy t)
```

**混合可选入参和关键字入参：**
```lisp
(defun hello (&optional name &key happy)
  (format t "hello ~a " name)
  (when happy
    (format t ":)~&")))
```
调用如下：
```lisp
(hello "me" :happy t)
;; hello me :)
;; NIL
```
## 默认值
```lisp
(defun hello (name &key (happy t))
```
## 不定参数
`&rest`:

```lisp
(defun mean (x &rest numbers)
    (/ (apply #'+ x numbers)
       (1+ (length numbers))))
```
调用如下：
```lisp
(mean 1)
(mean 1 2)
(mean 1 2 3 4 5)
```

在传递参数或者更高阶的操作时，我们可能需要通过 `&allow-other-keys` 来修正:
```lisp
(defun hello (name &key happy)
  (format t "hello ~a~&" name))

(hello "me" :lisper t)
;; => Error: unknown keyword argument
```
```lisp
(defun hello (name &key happy &allow-other-keys)
  (format t "hello ~a~&" name))

(hello "me" :lisper t)
;; hello me
```
这有一个真实的例子。 我们定义一个函数来打开一个始终使用 `:if-exists :supersede` 的文件，但仍将其他关键字传递给 `open` 函数。
```lisp
(defun open-supersede (f &rest other-keys &key &allow-other-keys)
  (apply #'open f :if-exists :supersede other-keys))
```
对于重复的 `:if-exists` 参数，我们优先选择第一个。

# 返回值
该函数的返回值是函数主体的最后执行返回的值。也有非标准的 `return-from <function name> <value>`，但几乎用不到。

Common Lisp 也支持返货多个值。

## 返回多值
返回多个值三个关键字：`values`, `multiple-value-bind`, `nth-value`。

注意：并非将多个值放入元组或列表返回。

### values

```lisp
(defun foo (a b c)
  a)
  
(defvar *res* (foo :a :b :c))
;; :A 
```

通过 `values` 返回多个值：
```lisp
(defun foo (a b c)
  (values a b c))

(setf *res* (foo :a :b :c))
;; :A

(foo :a :b :c)
;; :A
;; :B
;; :C
```

我们看到这里的 `*res*` 仍然是 `:A`。
使用 `foo` 的返回值的所有函数都无需更改，它们仍然可以工作。 如果我们返回了列表或数组，则将有所不同。

### multiple-value-bind

我们通过 `multiple-value-bind` 解构多个值（在 Slime 中简称为 `mvb` + TAB），我们可以通过 `nth-value` 获取其位置。
```lisp
(multiple-value-bind (res1 res2 res3)
    (foo :a :b :c)
  (format t "res1 is ~a, res2 is ~a, res2 is ~a~&" res1 res2 res3))
;; res1 is A, res2 is B, res2 is C
;; NIL
```

一般表示为：
```lisp
(multiple-value-bind (var-1 .. var-n) expr
  body)
```

### nth-value
```lisp
(nth-value 0 (values :a :b :c))  ;; => :A
(nth-value 2 (values :a :b :c))  ;; => :C
(nth-value 9 (values :a :b :c))  ;; => NIL

(nth-value 0 '(:a :b :c)) ;; => (:A :B :C)
(nth-value 1 '(:a :b :c)) ;; => NIL
```

### multiple-value-list
```lisp
(multiple-value-list (values 1 2 3))
;; (1 2 3)
```
### value-list
```lisp
(values-list '(1 2 3))
;; 1
;; 2
;; 3
```
# 匿名函数lambda
```lisp
(lambda (x) (print x))
```
```lisp
((lambda (x) (print x)) "hello")
;; hello
```

# 函数的调用
- **funcall**
- **apply**
```lisp
(funcall #'+ 1 2)

(apply #'+ '(1 2))
```

## 函数的引用
前面的例子使用 `#'`，但是用单引号 `'` 也是同样的，那么我们该用哪一个呢？

通常使用 `#'` 更安全，因为它尊崇 lexical scope：
```lisp
(defun  foo (x)
  (* x 100))
  
(flet ((foo (x) (1+ x)))
  (funcall #'foo 1))
;; => 2, as expected
;;
;; But:

(flet ((foo (x) (1+ x)))
  (funcall 'foo 1))
;; => 100
```

`#'` 是 `(function ...)` 的简写：
```lisp
(function +)
;; #<FUNCTION +>

(flet ((foo (x) (1+ x)))
  (print (function foo))
  (funcall (function foo) 1))
;; #<FUNCTION (FLET FOO) {1001C0ACFB}>
;; 2
```

# 高阶函数
高阶函数的返回值也是函数，必须使用 `funcall` 或者 `apply` 来调用它：
```lisp
(defun adder (n)
  (lambda (x) (+ x n)))
;; ADDER

(adder 5)
;; #<CLOSURE (LAMBDA (X) :IN ADDER) {100994ACDB}>
(funcall (adder 5) 3)
;; 8

((adder 3) 5)
In: (ADDER 3) 5
    ((ADDER 3) 5)
Error: Illegal function call.
```

事实上，CL对于函数和变量有不同的名称空间，也就是说，相同的名称可以引用不同的东西，这取决于它在计算表单中的位置。

```lisp
;; The symbol foo is bound to nothing:
CL-USER> (boundp 'foo)
NIL
CL-USER> (fboundp 'foo)
NIL
;; We create a variable:
CL-USER> (defparameter foo 42)
FOO
* foo
42
;; Now foo is "bound":
CL-USER> (boundp 'foo)
T
;; but still not as a function:
CL-USER> (fboundp 'foo)
NIL
;; So let's define a function:
CL-USER> (defun foo (x) (* x x))
FOO
;; Now the symbol foo is bound as a function too:
CL-USER> (fboundp 'foo)
T
;; Get the function:
CL-USER> (function foo)
#<FUNCTION FOO>
;; and the shorthand notation:
* #'foo
#<FUNCTION FOO>
;; We call it:
(funcall (function adder) 5)
#<CLOSURE (LAMBDA (X) :IN ADDER) {100991761B}>
;; and call the lambda:
(funcall (funcall (function adder) 5) 3)
8
```
为了简化一点，您可以认为CL中的每个符号(至少)有两个存储信息的 *cells*。一个单元格(有时称为其值单元格 *value cell*)可以保存绑定到该符号的值，您可以使用 `boundp` 来测试该符号是否绑定到值(在全局环境中)。可以使用 `symbol-value` 访问符号的值单元格。

另一个单元格——有时被称为它的函数单元格 *function cell* ——可以保存符号(全局)函数绑定的定义。在这种情况下，该符号被称为与这个定义绑定。可以使用 `fboundp` 测试符号是否被 fbound。您可以使用 `symbol-function` 访问符号的函数单元格(在全局环境中)。

现在，如果一个符号被计算，它被当作一个变量，因为它的值单元格被返回(只是 foo)。如果计算一个复合形式，即一个 cons，并且它的car是一个符号，则使用该符号的函数单元格(如 `(foo 3)`)。

与 Scheme 不同，在 Common Lisp 中，要计算的复合表单的 car 不可能是任意表单。如果它不是一个符号，它必须是一个 lambda 表达式，它看起来像 `(lambda lambda-list form*)`。

我们解释了上面得到的错误消息 —— `(adder 3)` 既不是符号也不是 lambda 表达式。

如果我们想在复合形式的car中使用符号 `*my-fun*`，我们必须明确地在它的函数单元格中存储一些东西(这通常是由宏 `defun` 完成的):
```lisp
;;; continued from above
CL-USER> (fboundp '*my-fun*)
NIL
CL-USER> (setf (symbol-function '*my-fun*) (adder 3))
#<CLOSURE (LAMBDA (X) :IN ADDER) {10099A5EFB}>
CL-USER> (fboundp '*my-fun*)
T
CL-USER> (*my-fun* 5)
8
```

阅读 [form evaluation](http://www.lispworks.com/documentation/HyperSpec/Body/03_aba.htm) 了解更多。

# 闭包
闭包允许捕获语法绑定：
```lisp
(let ((limit 3)
      (counter -1))
    (defun my-counter ()
      (if (< counter limit)
          (incf counter)
          (setf counter 0))))

(my-counter)
0
(my-counter)
1
(my-counter)
2
(my-counter)
3
(my-counter)
0
```
或者类似：
```lisp
(defun repeater (n)
  (let ((counter -1))
     (lambda ()
       (if (< counter n)
         (incf counter)
         (setf counter 0)))))

(defparameter *my-repeater* (repeater 3))
;; *MY-REPEATER*
(funcall *my-repeater*)
0
(funcall *my-repeater*)
1
(funcall *my-repeater*)
2
(funcall *my-repeater*)
3
(funcall *my-repeater*)
0
```

参考 [Practical Common Lisp](http://www.gigamonkeys.com/book/variables.html) 。

# 函数setf
函数名也可以是有两个符号的列表，第一个是 `setf`，另一个是参数的值：
```lisp
(defun (setf <name>) (new-value <other arguments>)
  body)
```

这种机制特别用于 CLOS 方法：
```lisp
(defparameter *current-name* ""
  "A global name.")

(defun hello (name)
  (format t "hello ~a~&" name))

(defun (setf hello) (new-value)
  (hello new-value)
  (setf *current-name* new-value)
  (format t "current name is now ~a~&" new-value))

(setf (hello) "Alice")
;; hello Alice
;; current name is now Alice
;; NIL
```

# 柯里化Currying
## 概念

如果你尝试过函数式编程就会对 [currying](https://en.wikipedia.org/wiki/Currying) 很熟悉，在我们读了最后一节之后，实现起来相当容易。
```lisp
CL-USER> (defun curry (function &rest args)
           (lambda (&rest more-args)
	           (apply function (append args more-args))))
CURRY
CL-USER> (funcall (curry #'+ 3) 5)
8
CL-USER> (funcall (curry #'+ 3) 6)
9
CL-USER> (setf (symbol-function 'power-of-ten) (curry #'expt 10))
#<Interpreted Function "LAMBDA (FUNCTION &REST ARGS)" {482DB969}>
CL-USER> (power-of-ten 3)
1000
```

## alexandria库
[Alexandria](https://common-lisp.net/project/alexandria/draft/alexandria.html#Data-and-Control-Flow) 库：

```lisp
(ql:quickload :alexandria)

(defun adder (foo bar)
  "Add the two arguments."
  (+ foo bar))

(defvar add-one (alexandria:curry #'adder 1) "Add 1 to the argument.")

(funcall add-one 10)  ;; => 11

(setf (symbol-function 'add-one) add-one)
(add-one 10)  ;; => 11
```

# 参考文档
- **functions:** http://www.lispworks.com/documentation/HyperSpec/Body/t_fn.htm#function
- **ordinary lambda lists:** http://www.lispworks.com/documentation/HyperSpec/Body/03_da.htm
- **multiple-value-bind:** http://clhs.lisp.se/Body/m_multip.htm
