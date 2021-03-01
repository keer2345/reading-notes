**[2. Lather, Rinse, Repeat: A Tour of the REPL](http://www.gigamonkeys.com/book/lather-rinse-repeat-a-tour-of-the-repl.html)**


In this chapter you'll set up your programming environment and write your first Common Lisp programs. We'll use the easy-to-install Lisp in a Box developed by Matthew Danish and Mikel Evins, which packages a Common Lisp implementation with Emacs, a powerful Lisp-aware text editor, and SLIME, a Common Lisp development environment built on top of Emacs.

- [Choosing a Lisp Implementation](#choosing-a-lisp-implementation)
- [Getting Up and Running with Lisp in a Box](#getting-up-and-running-with-lisp-in-a-box)
- [Free Your Mind: Interactive Programming](#free-your-mind-interactive-programming)
- [Experimenting in the REPL](#experimenting-in-the-repl)
- ["Hello, World," Lisp Style](#hello-world-lisp-style)
- [Saving Your Work](#saving-your-work)

# Choosing a Lisp Implementation

The Common Lisp standard is a contract between any Common Lisp vendor and Common Lisp programmers. The contract tells you that if you write a program that uses the features of the language the way they're described in the standard, you can count on your program behaving the same in any conforming implementation.

All the Lisp code in this book should work in any conforming Common Lisp implementation unless otherwise noted, and SLIME will smooth out some of the differences between implementations by providing us with a common interface for interacting with Lisp. The output shown in this book is from Allegro running on GNU/Linux; in some cases, other Lisp's may generate slightly different error messages or debugger output. 

# Getting Up and Running with Lisp in a Box
Since the Lisp in a Box packaging is designed to get new Lispers up and running in a first-rate Lisp development environment with minimum hassle, all you need to do to get it running is to grab the appropriate package for your operating system and the preferred Lisp from the Lisp in a Box Web site listed in [Chapter 32](chapter32.md) and then follow the installation instructions.

Since Lisp in a Box uses Emacs as its editor, you'll need to know at least a bit about how to use it. Perhaps the best way to get started with Emacs is to work through its built-in tutorial. 

# Free Your Mind: Interactive Programming

Environment:  http://keer2345.github.io/2021/02/27/lisp-of-slime-and-quicklisp/

# Experimenting in the REPL

# "Hello, World," Lisp Style

# Saving Your Work
touch `hello.lisp`
``` lisp
(defun hello-world ()
  (format t "Hello, world!"))
```
```
CL-USER> (load "~/workspace/lisp/practical-common-lisp/chapter02/hello.lisp")

CL-USER> (hello-world)
Hello, world!
NIL
```