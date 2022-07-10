# Coin-Toss

## Usage

## Installation

## Author

* keer (keer2345@gmail.com)

## Copyright

Copyright (c) 2022 keer (keer2345@gmail.com)

## License

Licensed under the LLGPL License.

# Project

- https://www.youtube.com/watch?v=3GEAINRCbJ4&list=PLCpux10P7KDKPb4eI5b_qSnQaY1ePGKGK&index=2

## Create

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
* (cl-project:make-project #p"./coin-toss/" :author "keer" :email "keer2345@gmail.com" :license "LLGPL")
writing ./coin-toss/coin-toss.asd
writing ./coin-toss/README.org
writing ./coin-toss/README.markdown
writing ./coin-toss/.gitignore
writing ./coin-toss/src/main.lisp
writing ./coin-toss/tests/main.lisp
T
```

## Knowledage Point
**Prep.**

```lisp
(format t "Hello world!~%")
(format t "Hello ~A ~A!~%" "Neil" "Mnuro")

(defun hello-world (fname lname)
  (format t "Hello ~A ~A!~%" fname lname))

(hello-world "Neil" "Mnuro")
```

**make-random-state**

Radom generate a value when `make-random-state` is `t`.

