# Demo01

## Usage

## Installation

## Author

* keer (keer2345@gmail.com)

## Copyright

Copyright (c) 2022 keer (keer2345@gmail.com)

## License

Licensed under the LLGPL License.

```sh
> rlwrap sbcl
This is SBCL 2.0.1.debian, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* (ql:quickload :cl-project)
To load "cl-project":
  Load 1 ASDF system:
    cl-project
; Loading "cl-project"
..
(:CL-PROJECT)
* (cl-project:make-project #p"./demo01/" :author "keer" :email "keer2345@gmail.com" :license "LLGPL")
writing ./demo01/demo01.asd
writing ./demo01/README.org
writing ./demo01/README.markdown
writing ./demo01/.gitignore
writing ./demo01/src/main.lisp
writing ./demo01/tests/main.lisp
T
```

`src/main.lisp`:
```lisp
(defpackage demo01
  (:use :cl))
(in-package :demo01)

;; blah blah blah.
(format t "Hello world!~%")

(defun hello-world (name)
  (format t "Hello, ~A!~%" name))

(hello-world "Neil")

;; (ql:quickload :clsql)
```
