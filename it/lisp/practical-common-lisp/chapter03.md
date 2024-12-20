# [3. Practical: A Simple Database](http://www.gigamonkeys.com/book/practical-a-simple-database.html)

## CDs and Records

列表：

``` lisp
CL-USER> (list 1 2 3)
(1 2 3)

CL-USER> (list :a 1 :b 2 :c 3)
(:A 1 :B 2 :C 3)

CL-USER> (getf (list :a 1 :b 2 :c 3) :a)
1

CL-USER> (getf (list :a 1 :b 2 :c 3) :c)
3
```
创建一个函数 **make-cd**

``` lisp
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))
```
``` lisp
CL-USER> (make-cd "Roses" "Kathy Mattea" 7 t)
(:TITLE "Roses" :ARTIST "Kathy Mattea" :RATING 7 :RIPPED T) 
```

## Filing CDs

我们需要使用宏 __DEFVAR__ 来创建一个全局变量（global variable）__`*db*`__：
``` lisp
(defvar *db* nil)
```
接着使用 **PUSH** 来添加记录到 `*db*`，先定义一个添加记录的函数 `add-record`：
```lisp
(defun add-record (cd) (push cd *db*))
```
``` lisp
CL-USER> (add-record (make-cd "Roses" "Kathy Mattea" 7 t))
((:TITLE "Roses" :ARTIST "Kathy Mattea" :RATING 7 :RIPPED T))

CL-USER> (add-record (make-cd "Fly" "Dixie Chicks" 8 t))
((:TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 8 :RIPPED T)
 (:TITLE "Roses" :ARTIST "Kathy Mattea" :RATING 7 :RIPPED T))

CL-USER> (add-record (make-cd "Home" "Dixie Chicks" 9 t))
((:TITLE "Home" :ARTIST "Dixie Chicks" :RATING 9 :RIPPED T)
 (:TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 8 :RIPPED T)
 (:TITLE "Roses" :ARTIST "Kathy Mattea" :RATING 7 :RIPPED T))
```

## Looking at the Database Contents

可以在 REPL 通过输入 `*db*` 来查看变量内容：
```lisp
CL-USER> *db*
((:TITLE "Home" :ARTIST "Dixie Chicks" :RATING 9 :RIPPED T)
 (:TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 8 :RIPPED T)
 (:TITLE "Roses" :ARTIST "Kathy Mattea" :RATING 7 :RIPPED T))
```

当然，我们要写一个函数 `dump-db` 来查看数据库内容：
``` lisp(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))
```
```
TITLE:    Home
ARTIST:   Dixie Chicks
RATING:   9
RIPPED:   T

TITLE:    Fly
ARTIST:   Dixie Chicks
RATING:   8
RIPPED:   T

TITLE:    Roses
ARTIST:   Kathy Mattea
RATING:   7
RIPPED:   T
```

**FORMAT** 的使用：

``` lisp
CL-USER> (format t "~a" "Dixie Chicks")
Dixie Chicks
NIL


CL-USER> (format t "~a" :title)
TITLE
NIL


CL-USER> (format t "~a:~10t~a" :artist "Dixie Chicks")
ARTIST:   Dixie Chicks
NIL


(defun dump-db ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))
```
## Improving the User Interaction
定义提示函数：
```lisp
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))
```

这里的 **FORMAT** 没有 `~%`，因此光标会保留在同一行。**FORCE-OUTPUT** 确保在打印提示信息之前不会等待换行。**READ-LINE** 读取输入。

```lisp
(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (prompt-read "Rating")
   (prompt-read "Ripped [y/n]")))
```

很好，但 `prompt-read` 函数返回的是字符串，如果要转换成整数就需要加入 **PARSE-INTEGER** ：
```lisp
(parse-integer (prompt-read "Rating"))
```
很不幸， **PARSE-INTEGER** 太加单了，如果字符串不是数字就会报错，因此要加入参数 `:junk-allowed` ：
```lisp
> (parse-integer "a10" :junk-allowed t)
NIL
0

> (parse-integer "10a" :junk-allowed t)
10
2
```
如果返回的是 `NIL`，我们默认将其看作是 `0`，**OR** 宏可以有多个参数，lisp 将逐一计算每一个参数，返回第一个非 `NIL` 的结果。
```lisp
> (or NIL NIL 1 2)
1

> (or (parse-integer "a10" :junk-allowed t) 0)
0

> (or (parse-integer "10a" :junk-allowed t) 0)
10
```
进一步改善后的 `prompt-for-cd`：
```lisp
(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))
```
使用循环，可以不断的加入我们的 CD 媒体库：
```lisp
(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? [y/n]: ")) (return))))
```

我们可以通过 `add-cds` 来添加媒体库了：
```lisp
CL-USER> (add-cds)
Title: Rockin' the Suburbs
Artist: Ben Folds
Rating: 6
Ripped [y/n]:  (y or n) y
Another? [y/n]:  (y or n) y
Title: Give Us a Break
Artist: Limpopo
Rating: 10
Ripped [y/n]:  (y or n) y
Another? [y/n]:  (y or n) y
Title: Lyle Lovett
Artist: Lyle Lovett
Rating: 9
Ripped [y/n]:  (y or n) n
Another? [y/n]:  (y or n) n
NIL
CL-USER>
```

## Saving and Loading the Database
定义函数 `save-db`：
```lisp
(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))  ;print宏用来将变量输出到流中
```
宏 **WITH-OPEN-FILE** 用来打开文件，绑定流到一个变量，执行一些操作，然后关闭文件。其中：
```
:direction :output;声明打开文件为了写
:if-exists :supersede;如果文件存在则重写
with-standard-io-syntax;表示标准的IO异常处理
```
`save-db` 的效果如下：
```lisp
CL-USER> (save-db "./my-cds.db")
((:TITLE "Lyle Lovett" :ARTIST "Lyle Lovett" :RATING 9 :RIPPED NIL)
 (:TITLE "Give Us a Break" :ARTIST "Limpopo" :RATING 10 :RIPPED T)
 (:TITLE "Rockin' the Suburbs" :ARTIST "Ben Folds" :RATING 6 :RIPPED T))
CL-USER>
```

加载数据：
```lisp
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))
```
运行后，就把数据加载出来了：
```lisp
CL-USER> (load-db "./my-cds.db")
((:TITLE "Lyle Lovett" :ARTIST "Lyle Lovett" :RATING 9 :RIPPED NIL)
 (:TITLE "Give Us a Break" :ARTIST "Limpopo" :RATING 10 :RIPPED T)
 (:TITLE "Rockin' the Suburbs" :ARTIST "Ben Folds" :RATING 6 :RIPPED T))
```


## Querying the Database
目前为止，已经可以保存和加载数据了。我们还需要查找数据，类似这样：
```lisp
(select :artist "Dixie Chicks")
```

函数 **REMOVE-IF-NOT** 包含谓语和列表，并返回符合谓语断言的元素列表。换句话说，它移除了不匹配该断言的所有元素。
不过 **REMOVE-IF-NOT** 并没有真的移除任何东西，它是创建了一个新的列表，原始列表并不受影响。

```lisp
CL-USER> (remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10))
(2 4 6 8 10)

(remove-if-not #'(lambda (x) (= 0 (mod x 3))) '(1 2 3 4 5 6 7 8 9 10))
(3 6 9)
```

函数 **GETF** 可以提取列表中的字段名，比如 `(getf cd :artist)` 可提取艺术家的名字。函数 **EQUAL** 判断元素是否相等。
```lisp
CL-USER> (load "simple-database.lisp")
T
CL-USER> (load-db "./my-cds.db")
((:TITLE "Lyle Lovett" :ARTIST "Lyle Lovett" :RATING 9 :RIPPED NIL)
 (:TITLE "Give Us a Break" :ARTIST "Limpopo" :RATING 10 :RIPPED T)
 (:TITLE "Rockin' the Suburbs" :ARTIST "Ben Folds" :RATING 6 :RIPPED T))
CL-USER> *db*
((:TITLE "Lyle Lovett" :ARTIST "Lyle Lovett" :RATING 9 :RIPPED NIL)
 (:TITLE "Give Us a Break" :ARTIST "Limpopo" :RATING 10 :RIPPED T)
 (:TITLE "Rockin' the Suburbs" :ARTIST "Ben Folds" :RATING 6 :RIPPED T))

CL-USER> (remove-if-not #'(lambda (cd) (equal (getf cd :artist) "Lyle Lovett")) *db*)
((:TITLE "Lyle Lovett" :ARTIST "Lyle Lovett" :RATING 9 :RIPPED NIL))
CL-USER>
```

我们可以写出通过艺术家查找唱片的函数：
```lisp
(defun select-by-artist (artist)
  (remove-if-not
    #'(lambda (cb) (equal (getf cd :artist) artist))
    *db))
```

我们需要写一个通用的查询函数 `select`：
```lisp
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))
```
```lisp
CL-USER> (select #'(lambda (cd) (equal (getf cd :artist) "Lyle Lovett")))
((:TITLE "Lyle Lovett" :ARTIST "Lyle Lovett" :RATING 9 :RIPPED NIL))

CL-USER> (select #'(lambda (cd) (equal (getf cd :title) "Rockin' the Suburbs")))
((:TITLE "Rockin' the Suburbs" :ARTIST "Ben Folds" :RATING 6 :RIPPED T))

CL-USER> 
```

但是，这看起来也很不友好，我们需要进一步完善匿名函数：
```lisp
(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))
```
```lisp
CL-USER> (select (artist-selector "Limpopo"))
((:TITLE "Give Us a Break" :ARTIST "Limpopo" :RATING 10 :RIPPED T))
CL-USER>
```


可是这样的话，又要写类似的 `select-by-title`, `select-by-rating` 等等，考虑到这些都是类似的，我们需要写一个通用的选择函数。

首先我们来了解一下基础知识：
```lisp
(defun foo (a b c)
  (list a b c))
```
```lisp
(defun foo (&key a b c)
  (list a b c))
```

后者的差异在于 `&key`：
```lisp
(foo :a 1 :b 2 :c 3)  ; (1 2 3)
(foo :c 3 :b 2 :a 1)  ; (1 2 3)
(foo :a 1 :c 3)       ; (1 NIL 3)
(foo)                 ; (NIL NIL NIL)
```

判断传递的参数是否为空：
```lisp
(defun foo (&key a (b 20) (c 30 c-p)) (list a b c c-p))

(foo :a 1 :b 2 :c 3)  ; (1 2 3 T)
(foo :c 3 :b 2 :a 1)  ; (1 2 3 T)
(foo :a 1 :c 3)       ; (1 20 3 T)
(foo)                 ; (NIL 20 30 NIL)
```

回到正题，通用的选择函数我们定义为 `where`，就像 SQL 里的一样。例如：
```lisp
(select (where :artist "Dixie Chicks"))
(select (where :rating 10 :ripped nil))
```

要构建的 `where` 函数看起来像这样：
```lisp
(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title    (equal (getf cd :title)  title)  t)
       (if artist   (equal (getf cd :artist) artist) t)
       (if rating   (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))
```
运行如下：
```lisp
CL-USER> *db*
((:TITLE "Lyle Lovett" :ARTIST "Lyle Lovett" :RATING 9 :RIPPED NIL)
 (:TITLE "Give Us a Break" :ARTIST "Limpopo" :RATING 10 :RIPPED T)
 (:TITLE "Rockin' the Suburbs" :ARTIST "Ben Folds" :RATING 6 :RIPPED T))
CL-USER> (select (where :artist "Lyle Lovett"))
((:TITLE "Lyle Lovett" :ARTIST "Lyle Lovett" :RATING 9 :RIPPED NIL))
CL-USER> (select (where :ripped t))
((:TITLE "Give Us a Break" :ARTIST "Limpopo" :RATING 10 :RIPPED T)
 (:TITLE "Rockin' the Suburbs" :ARTIST "Ben Folds" :RATING 6 :RIPPED T))
CL-USER> (select (where :ripped t :rating 10))
((:TITLE "Give Us a Break" :ARTIST "Limpopo" :RATING 10 :RIPPED T))
CL-USER>
```


## Updating Existing Records--Another Use for WHERE

到现在，我们构建了 `select` 和 `where` 函数。接下来构建 `update` 函数。

关于这部分，可以参考：
- https://stackoverflow.com/questions/38897093/practical-common-lisp-understanding-chapter-3

```lisp
(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title (setf (getf row :title) title))
               (if artist (setf (getf row :artist) artist))
               (if rating (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db* )))
```
运行：
```lisp
CL-USER> (update (where :artist "Limpopo") :title "Give Us a Break")
((:TITLE "Lyle Lovett" :ARTIST "Lyle Lovett" :RATING 9 :RIPPED NIL)
 (:TITLE "Give Us a Break" :ARTIST "Limpopo" :RATING 9 :RIPPED T)
 (:TITLE "Rockin' the Suburbs" :ARTIST "Ben Folds" :RATING 6 :RIPPED T))
CL-USER>
```


接下来，可以很容易地写出删除功能的函数：
```lisp
(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))
```
```lisp
CL-USER> *db*
((:TITLE "Rockin' the Suburbs" :ARTIST "Ben Folds" :RATING 6 :RIPPED T))
CL-USER> 
```

## Removing Duplication and Winning Big

我们的代码已经超过了 50 行，其中有一些是重复的代码，我们要消除重复，让代码更加的灵活。

Lisp 的一个特性就是宏，必须强调的是除了名字 **Macro** 和 C、C++ 的宏相同之外，实质上是不同的。

从一个简单的例子开始，展示如何通过 `where` 宏来替换 `where` 函数。在此之前，我先介绍一个新的函数：**REVERSE**，它有一个列表参数，
并返回一个反转的列表。比如 `(reverse '(1 2 3))` 的值为 `(3 2 1)`.
```lisp
(reverse '(1 2 3))  ; (3 2 1)
```

我们来定义一个宏：
```lisp
(defmacro backwards (expr) (reverse expr))
```
**DEFMACRO** 和 **DEFUN** 主要区别在于前者是用来定义宏的。宏的使用：
```lisp
CL-USER> (backwards ("hello, world" t format))
hello, world
NIL
```

它是如何工作的？翻转后 `(format t "hello, world")` ，再求值。

这有助于前面的 `where` 的重复代码吗？当然，我们可以为每个写个宏，为每个特定的调用来生成代码。同样，最好的方法是自底向上构建。

```
(equal (getf cd field) value)
```
我们创建一个函数，提供字段的名字和值，返回类似的表达式：
```lisp
(defun make-comparision-expr (field value)  ; 这是一个错误示例
  (list equal (list getf cd field) value)
```

上面的示例是错误的，正如我们知道的，Lisp 会尝试将 `equal`，`getf` 和 `cd` 作为变量，这不是我们想要的。要解决这个问题，只需要在前面加个单引号 `'a`，像这样：
```lisp
(defun make-comparison-expr (field value)
  (list 'equal (list 'getf 'cd field) value))
```
在 REPL 中测试：
```lisp
CL-USER> (make-comparison-expr :rating 10)
(EQUAL (GETF CD :RATING) 10)
CL-USER> (make-comparison-expr :title "Give Us a Break")
(EQUAL (GETF CD :TITLE) "Give Us a Break")
```

其实，还有更好的方法。像单引号那样，前面加上反引号（`` ` ``）来禁止求值:
```lisp
; SLIME 2.29.1
CL-USER> `(1 2 3)
(1 2 3)
CL-USER> '(1 2 3)
(1 2 3)
CL-USER> (equal `(1 2 3) '(1 2 3))
T
CL-USER> `(1 2 (+ 1 2))
(1 2 (+ 1 2))
CL-USER> `(1 2 ,(+ 1 2))
(1 2 3)
CL-USER>
```

这样的话，`make-comparison-expr` 函数可以改成：
```lisp
(defun make-comparison-expr (field value)
  `('equal (getf cd ,field) ,value))
```

再看看之前的函数，可以看到函数体由多个字段或值组成，包裹在 **AND** 表达式里。假设给 `where` 宏赋予简单列表的参数，将需要一个函数，
它可以成对获取列表中的元素，并手机调用 `make-comparison-expr` 返回的结果。为了实现这样一个函数，可以从高级的 Lisp 技巧中知道到
 **LOOP** 宏：
 ```lisp
(defun make-comparisons-list (fields)
  (loop while fields
        collecting (make-comparison-expr (pop fields) (pop fields))))
```

**LOOP** 会在 [第22章](ch22.md)详细讲解，这里只要知道 **LOOP** 表达式可以满足我们的需求：需要 `fields` 列表里的元素，每次
通过 `make-comparison-expr` 舍弃两个，并在循环结束后返回结果。宏 **POP** 是 **PUSH** 的逆操作。

定义 `where` 宏：

```lisp
(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))
```
```lisp
`(and ,(list 1 2 3))   ==> (AND (1 2 3))
`(and ,@(list 1 2 3))  ==> (AND 1 2 3)
`(and ,@(list 1 2 3) 4) ==> (AND 1 2 3 4)
```

这个列表被传递给了 `make-comparisons-list`，其返回一个由比较表达式所组成的列表。可以通过使用函数 `MACROEXPAND-1` 来精确地看到一个 `where` 调用将产生出哪些代码。如果传给 **MACROEXPAND-1** 一个代表宏调用的形式，它将使用适当的参数来调用宏代码并返回其展开式。因此可以像这样检查上一个`where` 调用：
```lisp
CL-USER> (macroexpand-1 '(where :title "Give Us a Break" :ripped t))
#'(LAMBDA (CD)
    (AND (EQUAL (GETF CD :TITLE) "Give Us a Break")
         (EQUAL (GETF CD :RIPPED) T)))
T

```
看起来不错。现在让我们实际试一下。
```lisp
CL-USER> (select (where :title "Give Us a Break" :ripped t))
((:TITLE "Give Us a Break" :ARTIST "Limpopo" :RATING 10 :RIPPED T))
```

现在，有趣的事情发生了。你不但去除了重复，而且还使得代码更有效且更通用了。这通常就是正确选用宏所达到的效果。这件事合乎逻辑，是因为宏只不过是另一种创建抽象的手法——词法层面的抽象，以及按照定义通过更简明地表达底层一般性的方式所得到的抽象。现在这个微型数据库的代码中只有 make-cd、prompt-for-cd 以及 add-cd 函数是特定于 CD 及其字段的。事实上，新的 where 宏可以用在任何基于 plist 的数据库上。



尽管如此，它距离一个完整的数据库仍很遥远。你可能会想到还有大量需要增加的特性，包括支持多表或是更复杂的查询。第 27 章将建立一个具备这些特性的 MP3 数据库。


本章的要点在于快速介绍少量 Lisp 特性，展示如何用它们编写出比 “hello, world” 更有趣一点儿的代码。在下一章里，我们将对 Lisp 做一个更加系统的概述。

## 总结
现在，有趣的事情发生了。你不但去除了重复，而且还使得代码更有效且更通用了。这通常就是正确选用宏所达到的效果。这件事合乎逻辑，是因为宏只不过是另一种创建抽象的手法——词法层面的抽象，以及按照定义通过更简明地表达底层一般性的方式所得到的抽象。现在这个微型数据库的代码中只有 make-cd、prompt-for-cd 以及 add-cd 函数是特定于 CD 及其字段的。事实上，新的 where 宏可以用在任何基于 plist 的数据库上。
