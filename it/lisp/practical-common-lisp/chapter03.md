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
