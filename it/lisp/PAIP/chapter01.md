**Chapter 01 [Introduction to Lisp](https://github.com/norvig/paip-lisp/blob/master/docs/chapter1.md)**

本章主要是给没有 Lisp 经验的程序员阅读的。计算机的使用者和程序员主要区别是前者是输入，后者定义新的操作、程序、甚至是数据类型。

推荐的环境：Sbcl + Quicklisp + Emacs + Slime

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [符号计算](#符号计算)
- [变量](#变量)
- [特殊形式](#特殊形式)
- [列表](#列表)
- [定义新函数](#定义新函数)
- [使用函数](#使用函数)
- [高阶函数](#高阶函数)
- [其他数据类型](#其他数据类型)
- [总结-Lisp评估规则](#总结-lisp评估规则)
- [Lisp如何与众不同](#lisp如何与众不同)
- [练习](#练习)

<!-- markdown-toc end -->


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
在 Lisp 中符号还用于命名函数。

# 特殊形式
`setf` 并不是一个函数，而是 Lisp 基本语法的一部分。除了原子和函数调用的语法外，Lisp 还有语法表达，他们是**特殊形式**（special forms）。它们的作用与其他编程语言中的语句相同，并且确实具有某些相同的语法标记，例如 `if` 和 `loop`。与其他语言的两个主要区别在于：Lisp 的语法形式都是列表，并且第一个元素拥有特权，比如 `(setf x 10)`；特殊形式还是返回值的表达式，这与其他语言正好相反，其他语言的表达式不会返回值。

| 函数            | 定义                                          |
|-----------------|----------------------------------------------|
| `defun`         | define function                              |
| `defparameter`  | define special variable                      |
| `setf`          | set variable or field to new value           |
| `let`           | bind local variable(s)                       |
| `case`          | choose one of several alternatives           |
| `if`            | do one thing or another, depending on a test |
| `function (#')` | refer to a function                          |
| `quote (')`     | introduce constant data                      |


# 列表
```lisp
> p => (JOHN Q PUBLIC)

> (first p) JOHN

> (rest p) => (Q PUBLIC)

> (second p) => Q

> (third p) => PUBLIC

> (fourth p) => NIL

> (length p) => 3
```
```lisp
> (setf x '((1st element) 2 (element 3) ((4)) 5))
((1ST ELEMENT) 2 (ELEMENT 3) ((4)) 5)

> (length x) => 5

> (first x) => (1ST ELEMENT)

> (second x) => 2

> (third x) => (ELEMENT 3)

> (fourth x) => ((4))

> (first (fourth x)) => (4)

> (first (first (fourth x))) => 4

> (fifth x) => 5

> (first x) => (1ST ELEMENT)

> (second (first x)) => ELEMENT
```
```lisp
> p => (JOHN Q PUBLIC)

> (cons 'Mr p) => (MR JOHN Q PUBLIC)

> (cons (first p) (rest p)) => (JOHN Q PUBLIC)

> (setf town (list 'Anytown 'USA)) => (ANYTOWN USA)

> (list p 'of town 'may 'have 'already 'won!) =>
((JOHN Q PUBLIC) OF (ANYTOWN USA) MAY HAVE ALREADY WON!)

> (append p '(of) town '(may have already won!)) =>
(JOHN Q PUBLIC OF ANYTOWN USA MAY HAVE ALREADY WON!)

> p => (JOHN Q PUBLIC)
```
```lisp
> (last p) => (PUBLIC)

> (first (last p)) => PUBLIC
```
# 定义新函数
通常文档部分是可选的，其他是必须的：
```lisp
(defun last-name (name)
  "Select the last name from a name represented as a list."
  (first (last name)))
```
比如，我们新建函数 `first-name`：
```lisp
(defun first-name (name)
  "Select the first name from a name represented as a list."
  (first name))

> p => (JOHN Q PUBLIC)`

> (first-name p) => JOHN`

> (first-name '(Wilma Flintstone)) => WILMA`

> (setf names '((John Q Public) (Malcolm X)
              (Admiral Grace Murray Hopper) (Spot)
              (Aristotle) (A A Milne) (Z Z Top)
              (Sir Larry Olivier) (Miss Scarlet))) =>

((JOHN Q PUBLIC) (MALCOLM X) (ADMIRAL GRACE MURRAY HOPPER)
 (SPOT) (ARISTOTLE) (A A MILNE) (Z Z TOP) (SIR LARRY OLIVIER)
 (MISS SCARLET))

> (first-name (first names)) => JOHN
```
# 使用函数
```lisp
> (mapcar #'last-name names)
(PUBLIC X HOPPER SPOT ARISTOTLE MILNE TOP OLIVIER SCARLET)
```
`#'` 的意思是从函数名映射到函数本身。`mapcar` 的调用等价于：
```lisp
(list (last-name (first names))
      (last-name (second names))
      (last-name (third names))
```

```lisp
> (mapcar #'- '(1 2 3 4)) => (-1 -2 -3 -4)

> (mapcar #'+ '(1 2 3 4) '(10 20 30 40)) => (11 22 33 44)
```
这个函数的缺陷是可能会忽略诸如显示、女士之类的头衔，我们继续完善：
```lisp
(defparameter *titles*
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General)
  "A list of titles that can appear at the start of a name.")
```
这里我们介绍了新的特殊形式 `defparameter`，它定义了在计算过程中不会被改变的参数变量，但是当我们想要添加更多的值时则会发生改变。`defparameter` 提供的值供我们在随后的函数定义中使用。实例中我们还提供了描述变量的字符串文档。这是 Lisp 程序员中广泛使用的约定，通过在两端用星号拼写它们的名称来标记特殊变量。 这只是一个惯例； 在 Lisp 中，星号只是另一个没有特殊含义的字符。


我们从新定义 `first-name`，如果第一个词是 `*titles*` 中的元素，则忽略它继续剩余的单词，否则取第一个元素。这里我们还用到了内建的 `member` 函数来查看第一个词是否为列表中的元素。

特殊形式 `if` 有这样的个格式：`(if *test then-part else-part*)`：
```lisp
(defun first-name (name)
  "Select the first name from a name represented as a list."
  (if (member (first name) *titles*)
      (first-name (rest name))
      (first name)))
```

执行如下：
```lisp
> (mapcar #'first-name names)
(JOHN MALCOLM GRACE SPOT ARISTOTLE A Z LARRY SCARLET)

> (first-name '(Madam Major General Paula Jones))
PAULA
```

可以通过 `trace` 和 `untrace` 来跟踪或者取消跟踪：
```lisp
> (trace first-name)
(FIRST-NAME)

> (first-name '(Madam Major General Paula Jones)) =>
(1 ENTER FIRST-NAME: (MADAM MAJOR GENERAL PAULA JONES))
  (2 ENTER FIRST-NAME: (MAJOR GENERAL PAULA JONES))
    (3 ENTER FIRST-NAME: (GENERAL PAULA JONES))
      (4 ENTER FIRST-NAME: (PAULA JONES))
      (4 EXIT FIRST-NAME: PAULA)
    (3 EXIT FIRST-NAME: PAULA)
  (2 EXIT FIRST-NAME: PAULA)
(1 EXIT FIRST-NAME: PAULA)
PAULA

> (untrace first-name) => (FIRST-NAME)

> (first-name '(Mr Blue Jeans)) => BLUE

> (untrace first-name)
```


# 高阶函数
将另一个函数作为入参的函数成为高阶函数（Higher-Order Function），例如 `mapcar`。为了演示高阶函数的样子，我们定义一个新的函数 `mappend`，它有两个入参，分别是函数和列表，`mappend` 遍历函数到列表中并添加到结果：
```lisp
(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))
```

我们先来看看 `apply` 是如何工作的：
```lisp
> (apply #'+ '(1 2 3 4)) => 10
```

再看看将 `append` 应用到两个列表（如果入参不是列表会报错）：
```lisp
> (apply #'append '((1 2 3) (a b c))) => (1 2 3 A B C)
```

现在定义函数 `self-and-double`，将其应用到入参中：
```lisp
> (defun self-and-double (x) (list x (+ x x)))

> (self-and-double 3) => (3 6)

> (apply #'self-and-double '(3)) => (3 6)
```

最后看看 `mappend` 函数的结果：
```lisp
> (mapcar #'self-and-double '(1 10 300)) => ((1 2) (10 20) (300 600))

> (mappend #'self-and-double '(1 10 300)) => (1 2 10 20 300 600)
```
现在考虑一个问题：给定一个列表，返回该列表的元素及其对应的负数值，例如 `(testing 1 2 3 test)`，返回 `(1 -1 2 -2 3 -3)`，用 `mappend` 很容易解决：
```lisp
(defun number-and-negation (x)
  "If x is a number, return a list of x and -x."
  (if (numberp x)
      (list x (- x))
      nil))

(defun numbers-and-negations (input)
  "Given a list, return only the numbers and their negations."
  (mappend #'number-and-negation input))

> (numbers-and-negations '(testing 1 2 3 test)) => (1 -1 2 -2 3 -3)
```

另一种不采用 `mapcar` 的 `mappend` 实现方式是采用 `funcall`：
```lisp
(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (if (null the-list)
      nil
      (append (funcall fn (first the-list))
              (mappend fn (rest the-list)))))
```
`funcall` 与 `apply` 很相似，它也调用函数作为第一个入参并将其应用到列表。但是以下情况的 `funcall` 参数是单独列出的：
```lisp
> (funcall #'+ 2 3) => 5

> (apply #' + '(2 3)) => 5

> (funcall #' + '(2 3)) => *Error: (2 3) is not a number.*
```
它们分别等价于 `(+ 2 3)`，`(+ 2 3)` 和 `(+ '(2 3))`，所以第三条报错了。

到目前为止我们介绍的 `defun` 都是有函数名的，还有一种是匿名函数，使用 `lambda` 表示，具体形式为：
```lisp
(lambda (*parameters...*) *body...*)
```

```lisp
> ((lambda (x) (+ x 2)) 4) => 6

> (funcall #'(lambda (x) (+ x 2)) 4) => 6
```
```lisp
>(mapcar #'(lambda (x) (+ x x))
         '(1 2 3 4 5)) =>
(2 4 6 8 10)

> (mappend #'(lambda (l) (list l (reverse l)))
           ((1 2 3) (a b c))) =>
((1 2 3) (3 2 1) (A B C) (C B A))
```

# 其他数据类型
到目前我们知道的类型有 number, symbol, list 和 function。Lisp 实际伤定义了 25 中不同类型的对象：vector, array, structure, character, stream, hash table 等等。我们说一下 string，它主要用来打印消息的输出，而 symbol 用于他们与对象的对应关系，也就是命名变量。字符串用双引号表示：
```lisp
> "a string" => "a string"

> (length "a string") => 8

> (length "") => 0
```


# 总结-Lisp评估规则
Lisp 的规则如下：
- 表达式也是 *list* 或者 *atom*。
- 列表也是特殊的表达式或者函数。
- 特殊形式表达式（special form expression）是由列表定义，列表的第一个元素为操作符。
- 函数也是列表，第一个参数为函数名。
- 原子（atom）要么是符号，要么不是符号。
- 一个符号的计算结果是已分配给该符号命名的变量的最新值。
- 非符号原子计算为自身。目前，数字和字符串是我们所知道的唯一的非符号原子。数字由数字组成，可能还有小数点和符号。也有科学的符号，有理数和复数，以及不同底数的规定，但我们不在这里描述细节。字符串由两边的双引号分隔。

# Lisp如何与众不同
八个方面让 Lisp 与其他编程语言与众不同：
- 默认的列表支持
- 自动存储管理
- 动态类型
- 函数第一
- 统一的语法
- 交互的环境
- 可扩展
- 历史

# 练习
- 新建一个 `last-name`，处理类似 "Rex Morgan MD," 和 "Morton Downey, Jr.," 的情况。
- 写一个指数函数，类似 `(power 3 2)`，即 3 的平方等于 9。
```lisp
(defun power (x n)
  "Power raises x to the nth power.  N must be an integer >= 0.
   This executes in log n time, because of the check for even n."
  (cond ((= n 0) 1)
        ((evenp n) (expt (power x (/ n 2)) 2))
        (t (* x (power x (- n 1))))))
```
- 计算表达式中原子的个数，例如 `(count-atoms '(a (b) c)) = 3`，需要考虑 `(a nil c)` 这样的情况。
```lisp
(defun count-atoms (exp)
  "Return the total number of non-nil atoms in the expression."
  (cond ((null exp) 0)
        ((atom exp) 1)
        (t (+ (count-atoms (first exp))
              (count-atoms (rest exp))))))

(defun count-all-atoms (exp &optional (if-null 1))
  "Return the total number of atoms in the expression,
  counting nil as an atom only in non-tail position."
  (cond ((null exp) if-null)
        ((atom exp) 1)
        (t (+ (count-all-atoms (first exp) 1)
              (count-all-atoms (rest exp) 0)))))
```
- 计算列表中元素出现的次数。
```lisp
(defun count-anywhere (item tree)
  "Count the times item appears anywhere within tree."
  (cond ((eql item tree) 1)
        ((atom tree) 0)
        (t (+ (count-anywhere item (first tree))
              (count-anywhere item (rest tree))))))
```
- 计算用列表表示的两个数字序列的点积，例如 `(dot-product '(10 20) '(3 4)) = 10 x 3 + 20 x 4 = 110`

三中方式：
```lisp
(defun dot-product (a b)
  "Compute the mathematical dot product of two vectors."
  (if (or (null a) (null b))
      0
      (+ (* (first a) (first b))
         (dot-product (rest a) (rest b)))))

(defun dot-product (a b)
  "Compute the mathematical dot product of two vectors."
  (let ((sum 0))
    (dotimes (i (length a))
      (incf sum (* (elt a i) (elt b i))))
    sum))

(defun dot-product (a b)
  "Compute the mathematical dot product of two vectors."
  (apply #'+ (mapcar #'* a b)))
```
