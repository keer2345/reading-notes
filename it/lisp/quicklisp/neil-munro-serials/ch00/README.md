## [Common Lisp Tutorial 0: Setup](https://www.youtube.com/watch?v=xyXDE5gP2QI&list=PLCpux10P7KDKPb4eI5b_qSnQaY1ePGKGK&index=1&pp=iAQB)

- [Quicklisp beta releases](https://www.quicklisp.org/beta/releases.html)
- [cl-project](https://github.com/fukamachi/cl-project)

```shell
rlwrap sbcl
* (ql:quickload :cl-project)
* (cl-project:make-project #p"demo-project"
    :author "keer2345"
    :email "keer2345@gmail.com"
    :license "LLGPL")
```
