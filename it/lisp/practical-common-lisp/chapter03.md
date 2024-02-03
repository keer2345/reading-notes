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

# Filing CDs

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

# Looking at the Database Contents

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

Technically, you could have also used **FORMAT** to loop over the database itself, turning our `dump-db2` function into a one-liner.

``` lisp
(defun dump-db2 ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))
```

# Improving the User Interaction
For now let's lean toward the quick and dirty: you can wrap the prompt-read for the rating in a call to Lisp's **PARSE-INTEGER** function.

Unfortunately, the default behavior of **PARSE-INTEGER** is to signal an error if it can't parse an integer out of the string or if there's any non-numeric junk in the string. However, it takes an optional keyword argument :junk-allowed, which tells it to relax a bit.

But there's still one problem: if it can't find an integer amidst all the junk, PARSE-INTEGER will return NIL rather than a number. In keeping with the quick-and-dirty approach, you may just want to call that 0 and continue. Lisp's OR macro is just the thing you need here. 
``` lisp
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? [y/n]: "))
            (return))))
```

# Saving and Loading the Database
``` lisp
(defun save-db (filename)
  (with-open-file (out filename
                       :direction
                       :output
                       :if-exists
                       :supersede)
    (with-standard-io-syntax
      (print *db* out))))
```

``` lisp
CL-USER> (save-db "~/my-cds.db")
((:TITLE "Lyle Lovett" :ARTIST "Lyle Lovett" :RATING 9 :RIPPED T)
 (:TITLE "Give Us a Break" :ARTIST "Limpopo" :RATING 10 :RIPPED T)
 (:TITLE "Rockin' the Suburbs" :ARTIST "Ben Folds" :RATING 6 :RIPPED
  T)
 (:TITLE "Home" :ARTIST "Dixie Chicks" :RATING 9 :RIPPED T)
 (:TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 8 :RIPPED T)
 (:TITLE "Roses" :ARTIST "Kathy Mattea" :RATING 9 :RIPPED T))
```
On Windows, the filename might be something like `"c:/my-cds.db"` or `"c:\\my-cds.db."`

# Querying the Database

Now that you have a way to save and reload the database to go along with a convenient user interface for adding new records, you soon may have enough records that you won't want to be dumping out the whole database just to look at what's in it. What you need is a way to query the database. You might like, for instance, to be able to write something like this:

``` lisp
(select :artist "Dixie Chicks")
```
For instance, if you wanted to extract all the even elements from a list of numbers, you could use **REMOVE-IF-NOT** as follows:
``` lisp
CL-USER> (remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10))
(2 4 6 8 10)
```
``` lisp
CL-USER> (remove-if-not #'(lambda (x) (= 0 (mod x 2))) '(1 2 3 4 5 6 7 8 9 10))
(2 4 6 8 10)
```
In this case, the predicate is this anonymous function:
``` lisp
(lambda (x) (= 0 (mod x 2)))
```
If you wanted to extract only the odd numbers using an anonymous function, you'd write this:
``` lisp
CL-USER> (remove-if-not #'(lambda (x) (= 1 (mod x 2))) '(1 2 3 4 5 6 7 8 9 10))
(1 3 5 7 9)
```

All you need to do is wrap that expression in a **LAMBDA** form to make an anonymous function and pass it to **REMOVE-IF-NOT**. 
``` lisp
CL-USER> (remove-if-not
  #'(lambda (cd) (equal (getf cd :artist) "Dixie Chicks")) *db*)
((:TITLE "Home" :ARTIST "Dixie Chicks" :RATING 9 :RIPPED T)
 (:TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 8 :RIPPED T))
```
Now suppose you want to wrap that whole expression in a function that takes the name of the artist as an argument. You can write that like this:

``` lisp
(defun select-by-artist (artist)
  (remove-if-not #'(lambda (cd) (equal (getf cd :artist) artist)) *db*))
```

You can instead make a more general **select** function that takes a function as an argument.
``` lisp
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))
``` 
``` lisp
CL-USER> (select #'(lambda (cd) (equal (getf cd :artist) "Dixie Chicks")))
((:TITLE "Home" :ARTIST "Dixie Chicks" :RATING 9 :RIPPED T)
 (:TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 8 :RIPPED T))
``` 

But that's really quite gross-looking. Luckily, you can wrap up the creation of the anonymous function.

``` lisp
(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))
``` 
``` lisp
CL-USER> (select (artist-selector "Dixie Chicks"))
((:TITLE "Home" :ARTIST "Dixie Chicks" :RATING 9 :RIPPED T)
 (:TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 8 :RIPPED T))
``` 

In the functions you've written so far, you've specified a simple list of parameters, which are bound to the corresponding arguments in the call to the function. For instance, the following function:

``` lisp
(defun foo (a b c) (list a b c))
``` 
has three parameters, `a`, `b`, and `c`, and must be called with three arguments. But sometimes you may want to write a function that can be called with varying numbers of arguments. Keyword parameters are one way to achieve this. A version of `foo` that uses keyword parameters might look like this:

``` lisp
(defun foo (&key a b c) (list a b c))
``` 
``` lisp
(foo :a 1 :b 2 :c 3)  ==> (1 2 3)
(foo :c 3 :b 2 :a 1)  ==> (1 2 3)
(foo :a 1 :c 3)       ==> (1 NIL 3)
(foo)                 ==> (NIL NIL NIL)
``` 
Normally if a function is called with no argument for a particular keyword parameter, the parameter will have the value NIL. However, sometimes you'll want to be able to distinguish between a NIL that was explicitly passed as the argument to a keyword parameter and the default value NIL. To allow this, when you specify a keyword parameter you can replace the simple name with a list consisting of the name of the parameter, a default value, and another parameter name, called a *supplied-p* parameter. The supplied-p parameter will be set to true or false depending on whether an argument was actually passed for that keyword parameter in a particular call to the function. Here's a version of foo that uses this feature:
``` lisp
(defun foo (&key a (b 20) (c 30 c-p)) (list a b c c-p))
``` 
``` lisp
(foo :a 1 :b 2 :c 3)  ==> (1 2 3 T)
(foo :c 3 :b 2 :a 1)  ==> (1 2 3 T)
(foo :a 1 :c 3)       ==> (1 20 3 T)
(foo)                 ==> (NIL 20 30 NIL)
``` 
The general selector-function generator, which you can call where for reasons that will soon become apparent if you're familiar with SQL databases, is a function that takes four keyword parameters corresponding to the fields in our CD records and generates a selector function that selects any CDs that match all the values given to where. For instance, it will let you say things like this:

``` lisp
(select (where :artist "Dixie Chicks"))
```

or this:

``` lisp
(select (where :rating 10 :ripped nil))
```

The function looks like this:

``` lisp
(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title    (equal (getf cd :title)  title)  t)
       (if artist   (equal (getf cd :artist) artist) t)
       (if rating   (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))
``` 
# Updating Existing Records--Another Use for WHERE
``` lisp
(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title    (setf (getf row :title) title))
               (if artist   (setf (getf row :artist) artist))
               (if rating   (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db*)))
```
``` lisp
CL-USER> (update (where :artist "Dixie Chicks") :rating 11)
NIL

CL-USER> (select (where :artist "Dixie Chicks"))
((:TITLE "Home" :ARTIST "Dixie Chicks" :RATING 11 :RIPPED T)
 (:TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 11 :RIPPED T))
``` 
You can even more easily add a function to delete rows from the database.
``` lisp
(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))
``` 
# Removing Duplication and Winning Big
For instance, if you found this snippet of code:
``` lisp
(select (where :title "Give Us a Break" :ripped t))
``` 

you could change it to this:

``` lisp
(select
 #'(lambda (cd)
     (and (equal (getf cd :title) "Give Us a Break")
          (equal (getf cd :ripped) t))))
```

The Lisp feature that makes this trivially easy is its macro system. 

I'll start with a simple, and silly, example and then show how you can replace the `where` function with a `where` macro. Before I can write this example macro, I need to quickly introduce one new function: `REVERSE` takes a list as an argument and returns a new list that is its reverse. So `(reverse '(1 2 3))` evaluates to `(3 2 1)`. Now let's create a macro. 
``` lisp
(defmacro backwards (expr) (reverse expr))
```
``` lisp
CL-USER> (backwards ("hello, world" t format))
hello, world
NIL
```

So if you write `make-comparison-expr` like this, it will do what you want: 
``` lisp
(defun make-comparison-expr (field value)
  (list 'equal (list 'getf 'cd field) value))
``` 

You can test it out in the REPL.

``` lisp
CL-USER> (make-comparison-expr :rating 10)
(EQUAL (GETF CD :RATING) 10)
CL-USER> (make-comparison-expr :title "Give Us a Break")
(EQUAL (GETF CD :TITLE) "Give Us a Break")
```
It turns out that there's an even better way to do it. What you'd really like is a way to write an expression that's mostly not evaluated and then have some way to pick out a few expressions that you do want evaluated. And, of course, there's just such a mechanism. A back quote (\`) before an expression stops evaluation just like a forward quote.
``` lisp
CL-USER> `(1 2 3)
(1 2 3)
CL-USER> '(1 2 3)
(1 2 3)
``` 
However, in a back-quoted expression, any subexpression that's preceded by a comma is evaluated. Notice the effect of the comma in the second expression:
``` lisp
`(1 2 (+ 1 2))        ==> (1 2 (+ 1 2))
`(1 2 ,(+ 1 2))       ==> (1 2 3)
```
Using a back quote, you can write `make-comparison-expr` like this: 
``` lisp
(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
     collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))
``` 
You can see the difference between `,` and `,@` in the following two expressions: 
``` lisp
`(and ,(list 1 2 3))   ==> (AND (1 2 3))
`(and ,@(list 1 2 3))  ==> (AND 1 2 3)
``` 
You can also use `,@` to splice into the middle of a list.
``` lisp
`(and ,@(list 1 2 3) 4)  ==> (AND 1 2 3 4)
``` 

The other important feature of the `where` macro is the use of `&rest` in the argument list. Like `&key`, `&rest` modifies the way arguments are parsed. With a `&rest` in its parameter list, a function or macro can take an arbitrary number of arguments, which are collected into a single list that becomes the value of the variable whose name follows the `&rest`. So if you call `where` like this:
``` lisp
(where :title "Give Us a Break" :ripped t)
``` 

the variable clauses will contain the list.

``` lisp
(:title "Give Us a Break" :ripped t)
``` 

This list is passed to `make-comparisons-list`, which returns a list of comparison expressions. You can see exactly what code a call to `where` will generate using the function **MACROEXPAND-1**. If you pass **MACROEXPAND-1**, a form representing a macro call, it will call the macro code with appropriate arguments and return the expansion. So you can check out the previous `where` call like this:

``` lisp
CL-USER> (macroexpand-1 '(where :title "Give Us a Break" :ripped t))
#'(LAMBDA (CD)
    (AND (EQUAL (GETF CD :TITLE) "Give Us a Break")
         (EQUAL (GETF CD :RIPPED) T)))
T
```

Looks good. Let's try it for real.

``` lisp
CL-USER> (select (where :title "Give Us a Break" :ripped t))
((:TITLE "Give Us a Break" :ARTIST "Limpopo" :RATING 10 :RIPPED T))
``` 
It works. And the where macro with its two helper functions is actually one line shorter than the old where function. And it's more general in that it's no longer tied to the specific fields in our CD records. 


# Wrapping Up

Now, an interesting thing has happened. You removed duplication and made the code more efficient and more general at the same time. That's often the way it goes with a well-chosen macro. This makes sense because a macro is just another mechanism for creating abstractions--abstraction at the syntactic level, and abstractions are by definition more concise ways of expressing underlying generalities. Now the only code in the mini-database that's specific to CDs and the fields in them is in the `make-cd`, `prompt-for-cd`, and `add-cd functions`. In fact, our new `where` macro would work with any plist-based database.

However, this is still far from being a complete database. You can probably think of plenty of features to add, such as supporting multiple tables or more elaborate queries. In Chapter 27 we'll build an MP3 database that incorporates some of those features.

The point of this chapter was to give you a quick introduction to just a handful of Lisp's features and show how they're used to write code that's a bit more interesting than "hello, world." In the next chapter we'll begin a more systematic overview of Lisp. 
