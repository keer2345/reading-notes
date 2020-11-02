> [目录](https://github.com/keer2345/reading-notes/issues/2#issue-733897659)

# 第一章 [开始](http://lispcookbook.github.io/cl-cookbook/getting-started.html)

# 安装
## 通过包管理器
```
apt-get install sbcl
```

以下版本也可能适合你的 Debian：
- [Steel Bank Common Lisp (SBCL)](http://www.sbcl.org/)
- [Embeddable Common Lisp (ECL)](https://gitlab.com/embeddable-common-lisp/ecl/), which compiles to C,
- [CLISP](https://clisp.sourceforge.io/)

还有：
- [ABCL](http://abcl.org/), to interface with the JVM,
- [ClozureCL](https://ccl.clozure.com/), a good implementation with very fast build times (see this [Debian package for Clozure CL](http://mr.gy/blog/clozure-cl-deb.html)),
- [CLASP](https://github.com/drmeister/clasp), that interoperates with C++ libraries using LLVM for compilation to native code,
- [AllegroCL](https://franz.com/products/allegrocl/) (proprietary)
- [LispWorks](http://www.lispworks.com/) (proprietary)

## 通过Roswell
[Roswell](https://github.com/roswell/roswell/wiki) 是一个：
- 多种实现的管理器：它可以很容易地安装 Common Lisp 实现（`ros install ecl`），以及指定版本号的实现（`ros install sbcl/1.2.0`），改变默认的实现（`ros use ecl`）。
- 脚本环境（有助于在 shell 中运行 Lisp，获取命令行参数……）
- 脚本安装器
- 测试环境（运行包括在流行的持续继承平台上的测试）
- 构建工具（以可移植的方式构建映像和可执行文件）

## 通过Docker
镜像 [daewok/lisp-devel-docker](https://github.com/daewok/lisp-devel-docker) 包括 SBCL, CCL, ECL 和 ABCL。
## Windoows
Windows 平台有以下三种选择：
- [Portacle](https://shinmera.github.io/portacle/)
- [ρEmacs](https://rho-emacs.sourceforge.io/)
- [Corman Lisp](https://github.com/sharplispers/cormanlisp)

# 开始REPL
```
user@debian:~$ sbcl
This is SBCL 1.4.16.debian, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* (+ 1 2)

3
* (quit)
user@debian:~$
```
您可以用 `rlwrap` 稍微增强REPL(比如箭头键不起作用，没有历史等)
```
apt-get install rlwrap
```
```
rlwrap sbcl
```

# Libraries
Common Lisp有成百上千的自由软件库：
*   [Quickdocs](http://quickdocs.org/) - the library documentation hosting for CL.
*  [Awesome-cl](https://github.com/CodyReichert/awesome-cl) list, a curated list of libraries.
*   [Cliki](http://www.cliki.net/), the Common Lisp wiki.

## 安装Quicklisp
[Quicklisp](https://www.quicklisp.org/beta/)
 不仅是软件包管理器，还是一个中央存储库（dist），可确保所有库一起构建。

它提供了自己的 *dist*，但也可以构建自己的 *dist*。我们可以通过以下两种方式安装：

**在任意地方执行命令**
```
curl -O https://beta.quicklisp.org/quicklisp.lisp
```
然后加载文件：
```
sbcl --load quicklisp.lisp
```

**通过包管理器安装**
```
apt-get install cl-quicklisp
```
在 REPL 加载文件：
```
(load "/usr/share/common-lisp/source/quicklisp/quicklisp.lisp")
```


上面两种方式都可以在 REPL 中：
```
(quicklisp-quickstart:install)
```

这里创建 `~/quicklisp` 目录，Quicklisp将在其中维护其状态并下载项目。

如果愿意，可以将Quicklisp安装到其他位置。 例如，要将其安装到Unix系统上的隐藏文件夹中：
```
(quicklisp-quickstart:install :path "~/.quicklisp")
```

如果希望始终将 Quicklisp 加载到 Lisp 会话中，请运行 `(ql:add-to-init-file)`：这会将正确的内容添加到 CL 实现的 init 文件中。 否则，如果要使用 Quicklisp 或通过它安装的任何库，则必须在每个会话中运行 `(load "~/ quicklisp / setup.lisp")`。

将以下你内容添加到类似你的 `~/.sbclrc` 文件：
``` shell
#-quicklisp
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init)))
```
## 安装库
在 REPL 中：

**加载库**
```
(ql:quickload "package-name")
```
**卸载库**
```
(ql:uninstall "package-name")
```
**查询库**
```
(ql:system-apropos "package-name")
```
**更新库**
```
(ql:update-dist "quicklisp")
```

**加载库**
```
(ql:quickload "package-name")
```

有关更多的命令，请查阅 Quicklisp 的文档。

还要注意，Debian 中打包了许多 Common Lisp 库，包名通常以 *cl* 开头（使用 `apt-cache search --names-only "^cl-.*"` 可以列出他们）。

例如，想要使用 CL-PPCRE 库（用于正则表达式），首先需要安装 *cl-ppcre* 包。

那么，在 SBCL 和 ECL 中可以通过这样使用：
``` lisp
(require "asdf")
(require "cl-ppcre")
(cl-ppcre:regex-replace "fo+" "foo bar" "frob")
```

更多可以查阅 See more: https://wiki.debian.org/CommonLisp 。

## 先进的依赖关系管理
可以将 Common Lisp 项目到以下任意文件夹：
- `~/common-lisp`
- `~/.local/share/common-lisp/source`
- `~/quicklisp/local-projects`

有关完整的列表，可以查看：
```
(asdf/source-registry:default-user-source-registry)
```
以及
```
asdf:*central-registry*
```


在这里安装的库可自动用于每个项目。

**提供我们自己版本库，克隆项目**

鉴于以上属性，我们可以将任何库克隆到 *local-projects* 目录中，ASDF（和 Quicklisp）会找到它，并且可以立即使用：
```
(asdf:load-system "system")
```
或者：
```
(ql:quickload "system")
```

两者之间的实际区别是 `ql:quickload` 会首先尝试从 Internet 上获取系统（如果尚未安装的话）。


**如何使用本地的版本库**

如果我们需要本地安装库，只针对一个项目，或者为了方便地为应用程序提供依赖项列表，我们可以使用 [Qlot](https://github.com/fukamachi/qlot)。

Quicklisp 还提供了 [Quicklisp bundles](https://www.quicklisp.org/beta/bundles.html)。它们是自包含的系统集，从 Quicklisp 导出，无需使用 Quicklisp 即可加载。

最后，还有 [Quicklisp controller](https://github.com/quicklisp/quicklisp-controller) 来帮助我们构建 *dists*。

# 项目实践
准备好 Quicklisp 和编辑器，就可以编写我们的 Lisp 代码在 REPL 交互了。

但是我们想要使用现有的项目或者创建新项目，该如何开始呢？`defpackage` 正确的顺序是什么？该把什么放到 `.asd` 文件中？如何将项目加载到 REPL？

## 创建新项目
一些项目构建者搭建好了项目的结构，我们喜欢设置好测试框架的 [cl-project](https://github.com/fukamachi/cl-project)。

简单的使用如下：
``` lisp
(ql:quickload "cl-project")
(cl-project:make-project #P"./path-to-project/my-project/")
```
创建后目录结构如下：
```
├── my-project.asd
├── README.markdown
├── README.org
├── src
│   └── main.lisp
└── tests
    └── main.lisp
```
