**Chapter 00 [Preface](https://github.com/norvig/paip-lisp/blob/master/docs/preface.md)**

# Preface
这本书涉及三个相关的主题：人工智能领域，计算机编程技能，以及编程语言 Common Lisp。

# Why Lisp? Why Common Lisp?
Liso 是最老的程序语言并在今天仍有广泛的使用。现在有许多的 Lisp 衍生版本，它们基本特性一样是细节不同。

首先，Lisp 在人工智能方面相当的流行，尤其是在美国。

其次，Lisp 在定义新对象时更容易捕获相关的泛化。特别是 Lisp 定义针对当前问题的新语言变得容易。这在人工智能应用程序中尤其方便，因为人工智能应用程序通常会处理最容易以某种新形式表示的复杂信息。Lisp 是少数允许在定义和操作程序以及数据方面具有完全灵活性的语言之一。根据定义，所有编程语言都提供了定义程序的方法，但是许多其他语言限制了程序的使用方式，或者限制了可以定义的程序的范围，或者要求程序员显式地声明无关的细节。

第三，Lisp 使快速开发工作程序变得非常容易。Lisp 程序是简洁的，没有底层的细节。Common Lisp 提供了大量有用的预定义对象，包括 700 多个函数。围绕 Lisp 系统的编程环境(如调试工具、增量编译器、集成编辑器和窗口系统接口) 通常非常好。而且 Lisp 的动态、交互式特性使得在开发程序时很容易试验和更改程序。

另外，Prolog 在欧洲和本越来越流行，本书的 11 章和 12 章介绍了 Prolog 的基本思想，并在 20、21 章使用了这些思想。

被称为 Scheme 的 Lisp 方言也越来越流行。22、23 章会介绍到。还有其他诸如 Franz Lisp, MacLisp, InterLisp, ZetaLisp, Standard Lisp 等被认为过时了的方言。最近唯一提出的 Lisp 新方言是 EuLisp，即欧洲 Lisp。Lisp 的一些方言作为嵌入式扩展语言继续存在。例如，Gnu Emacs 文本编辑器使用 elisp，而 AutoCad 计算机辅助设计包则使用 Xlisp 的派生工具 AutoLisp。 将来，Scheme 可能会成为一种流行的扩展语言，因为它虽然小巧但功能强大，并具有官方认可的标准定义。

由于具有灵活性，Lisp 已成功成为一种高级语言，可用于在 AI，图形和用户界面等领域进行快速原型制作。 Lisp 还是探索性编程的主要语言，那里的问题是如此复杂，以至于项目开始时没有明确的解决方案。 许多 AI 都属于此类别。

# How to Use This Book

这本书的目标读者很广泛:任何想成为高级Lisp程序员的人，以及任何想成为高级AI从业者的人。书中有几条推荐路径：
- *In an Introductory AI Course:* 注于第一部分和第二部分，以及第四部分中的至少一个示例。
- *In an Advanced AI Programming Course:* 专注于第一，第二和第四部分，跳过不那么感兴趣的章节，并在时间允许的情况下增加第三部分的内容。
- *In an Advanced Programming Languages Course:* 注于第一部分和第五部分，以及第三部分的选择。 如果相似的材料未与其他文字一起出现，请覆盖第11和13章。
- *For the Professional Lisp Programmer:* 尽可能多地阅读本书，并经常参考。 第三部分和第二十五章特别重要。

# 参考书目
- *Common Lisp*: A Gentle Introduction to Symbolic Computation by David Touretzky.
- *A Programmer's Guide to Common Lisp* by Deborah G. Tatar. 
- *Common LISPcraft* by Robert Wilensky. 
- *Common Lisp* by Wade L. Hennessey.
- *LISP* (3d edition) by Patrick H. Winston and Bertold Horn.
- *Programming Paradigms in Lisp* by Rajeev Sangal.
- *Programming for Artificial Intelligence* by Wolfgang Kreutzer and Bruce McKenzie.
- *Artificial Intelligence Programming* (2d edition) by Eugene Charniak, Christopher Riesbeck, Drew McDermott, and James Meehan.
- *AI in Practice*: Examples in Pop-11 by Allan Ramsey and Rosalind Barrett.
