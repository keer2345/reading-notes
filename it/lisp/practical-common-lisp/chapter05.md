**[5. Functions](http://www.gigamonkeys.com/book/functions.html)**

- [Defining New Functions](#defining-new-functions)
- [Function Parameter Lists](#function-parameter-lists)
- [Optional Parameters](#optional-parameters)
- [Rest Parameters](#rest-parameters)
- [Keyword Parameters](#keyword-parameters)
- [Mixing Different Parameter Types](#mixing-different-parameter-types)
- [Function Return Values](#function-return-values)
- [Functions As Data, a.k.a. Higher-Order Functions](#functions-as-data-aka-higher-order-functions)
- [Anonymous Functions](#anonymous-functions)

# Defining New Functions
``` lisp
(defun name (parameter*)
  "Optional documentation string."
  body-form*)

(defun hello-world () (format t "hello, world"))

(format t "hello, world")

(defun verbose-sum (x y)
  "Sum any two numbers after printing a message."
  (format t "Summing ~d and ~d.~%" x y)
  (+ x y))
```
# Function Parameter Lists
# Optional Parameters
``` lisp
(defun foo (a b &optional c d) (list a b c d))

(foo 1 2)     ==> (1 2 NIL NIL)
(foo 1 2 3)   ==> (1 2 3 NIL)
(foo 1 2 3 4) ==> (1 2 3 4)
``` 

Of course, you'll often want a different default value than NIL. 

``` lisp
(defun foo (a &optional (b 10)) (list a b))

(foo 1 2) ==> (1 2)
(foo 1)   ==> (1 10)
``` 

Sometimes, however, you may need more flexibility in choosing the default value. You may want to compute a default value based on other parameters. And you can--the default-value expression can refer to parameters that occur earlier in the parameter list. 
``` lisp
(defun make-rectangle (width &optional (height width)) ...)
```

Occasionally, it's useful to know whether the value of an optional argument was supplied by the caller or is the default value. 

``` lisp
(defun foo (a b &optional (c 3 c-supplied-p))
  (list a b c c-supplied-p))
``` 
``` lisp
(foo 1 2)   ==> (1 2 3 NIL)
(foo 1 2 3) ==> (1 2 3 T)
(foo 1 2 4) ==> (1 2 4 T)
``` 
# Rest Parameters
``` lisp
(defun format (stream string &rest values) ...)
(defun + (&rest numbers) ...) 
``` 
# Keyword Parameters
``` lisp
(defun foo (&key a b c) (list a b c))
``` 

``` lisp
(foo)                ==> (NIL NIL NIL)
(foo :a 1)           ==> (1 NIL NIL)
(foo :b 1)           ==> (NIL 1 NIL)
(foo :c 1)           ==> (NIL NIL 1)
(foo :a 1 :c 3)      ==> (1 NIL 3)
(foo :a 1 :b 2 :c 3) ==> (1 2 3)
(foo :a 1 :c 3 :b 2) ==> (1 2 3)
``` 
``` lisp
(defun foo (&key (a 0) (b 0 b-supplied-p) (c (+ a b)))
  (list a b c b-supplied-p))

(foo :a 1)           ==> (1 0 1 NIL)
(foo :b 1)           ==> (0 1 1 T)
(foo :b 1 :c 4)      ==> (0 1 4 T)
(foo :a 2 :b 1 :c 4) ==> (2 1 4 T)
``` 

Also, if for some reason you want the keyword the caller uses to specify the parameter to be different from the name of the actual parameter, you can replace the parameter name with another list containing the keyword to use when calling the function and the name to be used for the parameter. The following definition of foo:

``` lisp
(defun foo (&key ((:apple a)) ((:box b) 0) ((:charlie c) 0 c-supplied-p))
  (list a b c c-supplied-p))
``` 
lets the caller call it like this:

``` lisp
(foo :apple 10 :box 20 :charlie 30) ==> (10 20 30 T)
``` 

This style is mostly useful if you want to completely decouple the public API of the function from the internal details, usually because you want to use short variable names internally but descriptive keywords in the API. It's not, however, very frequently used. 

# Mixing Different Parameter Types

Combining `&optional` and `&key` parameters yields surprising enough results that you should probably avoid it altogether. The problem is that if a caller doesn't supply values for all the optional parameters, then those parameters will eat up the keywords and values intended for the keyword parameters. For instance, this function unwisely mixes `&optional` and `&key` parameters:

``` lisp
(defun foo (x &optional y &key z) (list x y z))
``` 

If called like this, it works fine:

``` lisp
(foo 1 2 :z 3) ==> (1 2 3)
``` 

And this is also fine:

``` lisp
(foo 1)  ==> (1 nil nil)
``` 

But this will signal an error:

``` lisp
(foo 1 :z 3) ==> ERROR
``` 

You can safely combine `&rest` and `&key` parameters, but the behavior may be a bit surprising initially. Normally the presence of either `&rest` or `&key` in a parameter list causes all the values remaining after the required and `&optional` parameters have been filled in to be processed in a particular way--either gathered into a list for a `&rest` parameter or assigned to the appropriate `&key` parameters based on the keywords. If both `&rest` and `&key` appear in a parameter list, then both things happen--all the remaining values, which include the keywords themselves, are gathered into a list that's bound to the `&rest` parameter, and the appropriate values are also bound to the `&key` parameters. So, given this function:

``` lisp
(defun foo (&rest rest &key a b c) (list rest a b c))
``` 

you get this result:

``` lisp
(foo :a 1 :b 2 :c 3)  ==> ((:A 1 :B 2 :C 3) 1 2 3)
``` 

# Function Return Values
All the functions you've written so far have used the default behavior of returning the value of the last expression evaluated as their own return value. This is the most common way to return a value from a function.

However, sometimes it's convenient to be able to return from the middle of a function such as when you want to break out of nested control constructs. In such cases you can use the **RETURN-FROM** special operator to immediately return any value from the function.

``` lisp
(defun foo (n)
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
        (return-from foo (list i j))))))
```
# Functions As Data, a.k.a. Higher-Order Functions
``` lisp
CL-USER> (defun foo (x) (* 2 x))
FOO

CL-USER> (function foo)
#<Interpreted Function FOO>

CL-USER> #'foo
#<Interpreted Function FOO>
``` 
Common Lisp provides two functions for invoking a function through a function object: **FUNCALL** and **APPLY**. They differ only in how they obtain the arguments to pass to the function. 

FUNCALL is the one to use when you know the number of arguments you're going to pass to the function at the time you write the code. The first argument to FUNCALL is the function object to be invoked, and the rest of the arguments are passed onto that function. Thus, the following two expressions are equivalent:

``` lisp
(foo 1 2 3) === (funcall #'foo 1 2 3)
``` 

However, there's little point in using **FUNCALL** to call a function whose name you know when you write the code. In fact, the previous two expressions will quite likely compile to exactly the same machine instructions.

The following function demonstrates a more apt use of **FUNCALL**. It accepts a function object as an argument and plots a simple ASCII-art histogram of the values returned by the argument function when it's invoked on the values from `min` to `max`, stepping by `step`.

``` lisp
(defun plot (fn min max step)
  (loop for i from min to max by step do
        (loop repeat (funcall fn i) do (format t "*"))
        (format t "~%")))
``` 

``` lisp
CL-USER> (plot #'exp 0 4 1/2)
*
*
**
****
*******
************
********************
*********************************
******************************************************
NIL
```

**APPLY**

That's where **APPLY** comes in. Like **FUNCALL**, the first argument to **APPLY** is a function object. But after the function object, instead of individual arguments, it expects a list. It then applies the function to the values in the list. This allows you to write the following instead:

``` lisp
(apply #'plot plot-data)
``` 

As a further convenience, **APPLY** can also accept "loose" arguments as long as the last argument is a list. Thus, if `plot-data` contained just the min, max, and step values, you could still use **APPLY** like this to plot the **EXP** function over that range:

``` lisp
(apply #'plot #'exp plot-data)
``` 

**APPLY** doesn't care about whether the function being applied takes **&optional, &rest**, or **&key** arguments--the argument list produced by combining any loose arguments with the final list must be a legal argument list for the function with enough arguments for all the required parameters and only appropriate keyword parameters. 

# Anonymous Functions
``` lisp
(lambda (parameters) body)

(funcall #'(lambda (x y) (+ x y)) 2 3) ==> 5

((lambda (x y) (+ x y)) 2 3) ==> 5

(defun double (x) (* 2 x))

CL-USER> (plot #'double 0 10 1)

**
****
******
********
**********
************
**************
****************
******************
********************
NIL

CL-USER> (plot #'(lambda (x) (* 2 x)) 0 10 1)

**
****
******
********
**********
************
**************
****************
******************
********************
NIL
```

The other important use of **LAMBDA** expressions is in making closures, functions that capture part of the environment where they're created. You used closures a bit in Chapter 3, but the details of how closures work and what they're used for is really more about how variables work than functions, so I'll save that discussion for the next chapter. 