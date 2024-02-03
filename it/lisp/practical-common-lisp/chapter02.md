# [2. Lather, Rinse, Repeat: A Tour of the REPL](http://www.gigamonkeys.com/book/lather-rinse-repeat-a-tour-of-the-repl.html)

## 交互编程
```lisp
CL-USER> 10
10

CL-USER> (+ 2 3)
5

CL-USER> "hello, world"
"hello, world"

CL-USER> (format t "hello, world")
hello, world
NIL

CL-USER> (defun hello-world () (format t "hello, world"))
HELLO-WORLD

CL-USER> (hello-world)
hello, world
NIL
```

## 保存
```lisp
CL-USER> (load "hello.lisp")
; Loading /home/peter/my-lisp-programs/hello.lisp
T

CL-USER> (hello-world)
Hello, world!
NIL

CL-USER> (load (compile-file "hello.lisp"))
;;; Compiling file hello.lisp
;;; Writing fasl file hello.fasl
;;; Fasl write complete
; Fast loading /home/peter/my-lisp-programs/hello.fasl
T
