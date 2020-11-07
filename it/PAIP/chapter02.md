**Chapter 02 [A Simple  Lisp Program](https://github.com/norvig/paip-lisp/blob/master/docs/chapter2.md)**

通过学习词汇表，您将永远不会精通外语。 相反，您必须听和说（或读和写）该语言才能熟练。 学习计算机语言也是如此。

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [英语子集的语法](#英语子集的语法)
- [直接的解决方案](#直接的解决方案)
- [基于规则的解决方案](#基于规则的解决方案)
    - [练习](#练习)
- [双管齐下](#双管齐下)
- [在不改变程序的情况下改变语法](#在不改变程序的情况下改变语法)
- [对多个程序使用相同的数据](#对多个程序使用相同的数据)
- [练习](#练习-1)

<!-- markdown-toc end -->

# 英语子集的语法
下面是一小部分简单的英语语法：
```
Sentence => Noun-Phrase + Verb-Phrase
Noun-Phrase => Article + Noun
Verb-Phrase => Verb + Noun-Phrase
Article => the, a,...
Noun => man, ball, woman, table...
Verb => hit, took, saw, liked...
```

从技术上讲，这种描述称为上下文无关的短语结构语法，而潜在的范例称为生成语法。这个想法是，在任何我们想要一个句子的地方，我们可以生成一个名词短语和一个动词短语。在指定名词短语的任何地方，我们生成后跟名词的冠词。在指定了项目的任何地方，我们生成“the”、“a”或其他项目。形式主义是“上下文无关”的，因为规则适用于任何地方，而不管周围的词，而方法是“生成”的，因为规则作为一个整体定义了一种语言中完整的一组句子(对比之下，非句子的一组也一样)。下面我们用这些规则推导出一个句子:

- 要得到一个句子，添加一个名词短语（Noun-Phrase）和一个动词短语（Verb-Phrase）
  - 要得到名词短语，则添加冠词（Article）和名词（Noun）
    - 选择 *the* 作为冠词
    - 选择 *man* 作为名词
  - 结果得到名词短语 *the man*
  - 要得到动词短语（Verb-Phrase），则添加动词（Verb）和名词短语（Noun-Phrase）
    - 选择 *hit* 作为动词
    - 名词短语 —— 添加冠词和名词
      - 冠词 *the*
      - 名词 *ball*
    - 得到名词短语 *the ball*
  - 得到动词短语 *hit the ball*
- 结果得到句子 *The man hit the ball*

# 直接的解决方案
我们要开发的程序是从语法段落结构生成随机的句子。最直接的方法是：
```lisp
(defun sentence ()    (append (noun-phrase) (verb-phrase)))
(defun noun-phrase () (append (Article) (Noun)))
(defun verb-phrase () (append (Verb) (noun-phrase)))
(defun Article ()     (one-of '(the a)))
(defun Noun ()        (one-of '(man ball woman table)))
(defun Verb ()        (one-of '(hit took saw liked)))
```

我们来定义 `one-of` 函数：
```lisp
(defun one-of (set)
  "Pick one element of set, and make a list of it."
  (list (random-elt set)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))
```

函数 `(elt list n)` 表示取列表的第 `n-1` 个元素（从 0 开始），`(random n)` 从 `0` 到 `n-1` 的随机整数 

现在，我们可以组成句子了：
```lisp
> (sentence) => (THE WOMAN HIT THE BALL)

> (sentence) => (THE WOMAN HIT THE MAN)

> (sentence) =>(THE BALL SAW THE WOMAN)

> (sentence) => (THE BALL SAW THE TABLE)

> (noun-phrase) => (THE MAN)

> (verb-phrase) => (LIKED THE WOMAN)

> (trace sentence noun-phrase verb-phrase article noun verb) =>
(SENTENCE NOUN-PHRASE VERB-PHRASE ARTICLE NOUN VERB)

> (sentence) =>
(1 ENTER SENTENCE)
  (1 ENTER NOUN-PHRASE)
    (1 ENTER ARTICLE)
    (1 EXIT ARTICLE: (THE))
    (1 ENTER NOUN)
    (1 EXIT NOUN: (MAN))
  (1 EXIT NOUN-PHRASE: (THE MAN))
  (1 ENTER VERB-PHRASE)
    (1 ENTER VERB)
    (1 EXIT VERB: (HIT))
    (1 ENTER NOUN-PHRASE)
      (1 ENTER ARTICLE)
      (1 EXIT ARTICLE: (THE))
      (1 ENTER NOUN)
      (1 EXIT NOUN: (BALL))
    (1 EXIT NOUN-PHRASE: (THE BALL))
  (1 EXIT VERB-PHRASE: (HIT THE BALL))
(1 EXIT SENTENCE: (THE MAN HIT THE BALL))
(THE MAN HIT THE BALL)
```

不错，但是我们可以更加深入，想象一下我们继续加入形容词（Adj）和介词（Prep）短语：
```
Noun-Phrase => Article + Adj* + Noun + PP*
Adj* => 0̸, Adj + Adj*
PP* => 0̸, PP + PP*
PP => Prep + Noun-Phrase
Adj => big, little, blue, green, ...
Prep => to, in, by, with, ...
```

```lisp
(defun Adj* ()
  (if (= (random 2) 0)
      nil
      (append (Adj) (Adj*))))

(defun PP* ()
  (if (random-elt '(t nil))
      (append (PP) (PP*))
      nil))

(defun noun-phrase () (append (Article) (Adj*) (Noun) (PP*)))
(defun PP () (append (Prep) (noun-phrase)))
(defun Adj () (one-of '(big little blue green adiabatic)))
(defun Prep () (one-of '(to in by with on)))
```

# 基于规则的解决方案
这个程序的另一种实现将专注于简化语法规则的编写，然后再考虑如何处理它们。让我们再来看看原来的语法规则:
```
Sentence => Noun-Phrase + Verb-Phrase
Noun-Phrase => Article + Noun
Verb-Phrase => Verb + Noun-Phrase
Article => the, a, ...
Noun => man, ball, woman, table...
Verb => hit, took, saw, liked...
```
规则可以表示如下：
```lisp
(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defvar *grammar* *simple-grammar*
  "The grammar used by generate.  Initially, this is
  *simple-grammar*, but we can switch to other grammars.")
```

这里引入了 `defvar` 和 `defparameter`。它们的区别在于，在运行程序的过程中会定期更改变量（如 *grammar*）。 另一方面，像 *simple-grammar* 这样的参数通常将保持不变。 对参数的更改被视为对程序的更改，而不是对程序本身的更改。

一旦定义了规则列表，就可以使用它来查找对给定类别符号可能进行的重写。`assoc` 函数就是为这类任务而设计的。它接受两个参数，`key` 和元素为列表的列表，并返回以 `key` 开始的列表中的第一个元素。如果没有，它返回 `nil`。下面是一个例子:
```lisp
> (assoc 'noun *grammar*) => (NOUN -> MAN BALL WOMAN TABLE)
```

尽管规则很简单地实现为列表，但通过定义对规则进行操作的函数来强加一个抽象层是个好主意。我们将需要三个函数:一个用于获取规则的右侧，一个用于获取规则的左侧，还有一个用于查找一个类别的所有可能的重写(右侧)。

```lisp
(defun rule-lhs (rule)
  "The left hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))
```
我们准备解决主要问题，定义生成句子的函数 `generate`，它包括三点：
1. 在最简单的情况下，`generate` 传递一个具有一组与之关联的重写规则的符号。我们随机选择其中一个，然后从中生成。
1. 如果符号没有可能的重写规则，它必须是一个终端符号——一个单词，而不是一个语法分类——我们希望它不受影响。实际上，我们返回的是输入单词的列表，因为在前面的程序中，我们希望所有结果都是单词列表。
1. 在某些情况下，当符号被重写时，我们将选择一个符号列表，并尝试从中生成。因此，`generate` 还必须接受一个列表作为输入，在这种情况下，它应该生成列表的每个元素，然后将它们全部附加在一起。在下面的例子中，`generate` 中的第一个子句处理这种情况，而第二个子句处理(1)，第三个句句处理(2)。

请注意，我们使用了前面的 `mappend` 函数：

```lisp
(defun generate (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((rewrites phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))
```
像这本书中的许多程序一样，这个函数很短，但信息丰富:编程的技巧包括知道什么不该写，什么不该写。

```lisp
> (generate 'sentence) => (THE TABLE SAW THE BALL)

> (generate 'sentence) => (THE WOMAN HIT A TABLE)

> (generate 'noun-phrase) => (THE MAN)

> (generate 'verb-phrase) (TOOK A TABLE)
```

有多种可能的方式来编写生成。 以下版本使用 `if` 代替 `cond`：
```lisp
(defun generate (phrase)
  "Generate a random sentence or phrase"
  (if (listp phrase)
      (mappend #'generate phrase)
      (let ((choices (rewrites phrase)))
        (if (null choices)
            (list phrase)
            (generate (random-elt choices))))))
```

此版本使用特殊形式的 `let`，它引入了一个新变量（在这种情况下为 `choices`）并将该变量绑定到一个值。 在这种情况下，引入变量可以避免调用两次函数重写，就像在 `cond` 版本的 `generate` 中一样。 `let` 形式的一般形式是：
```lisp
 `(let` ((*var value*)...)
        *body-containing-vars*)
```

## 练习
- 自己写个  `generate` 函数，使用 `cond` 但是两次调用 `rewrites`：
```lisp
(defun generate (phrase)
"Generate a random sentence or phrase"
(let ((choices nil))
  (cond ((listp phrase)
      (mappend #'generate phrase))
     ((setf choices (rewrites phrase))
      (generate (random-elt choices)))
     (t (list phrase)))))
```
- 再编写一个版本的 `generate`，明确区分终端符号（那些没有重写规则的）和非终端符号。
```lisp
(defun generate (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((non-terminal-p phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))

(defun non-terminal-p (category)
  "True if this is a category in the grammar."
  (not (null (rewrites category))))
```
# 双管齐下
前一个程序的两个版本代表了在开发程序时经常出现的两种替代方法：
1. 使用问题描述的最直接映射到 Lisp 代码中。
1. 使用最自然的符号来解决问题，然后再考虑为该符号编写解释器。

方法 2 涉及一个额外的步骤，因此对于小问题是更多的工作。然而，使用这种方法的程序通常更容易修改和扩展。在需要处理大量数据的领域中尤其如此。自然语言的语法就是这样一个领域——事实上，大多数AI问题都符合这种描述。方法 2 背后的思想是尽可能用它自己的术语处理问题，并尽量减少直接用 Lisp 编写的解决方案部分。

幸运的是，在 Lisp 中设计新的注释非常容易——实际上就是新的编程语言。因此，Lisp 鼓励构建更健壮的程序。在本书中，我们将了解这两种方法。读者可能会注意到，在大多数情况下，我们会选择第二种。

# 在不改变程序的情况下改变语法
我们通过定义一种包括形容词、介词短语、专有名词和代词的新语法来说明方法 2 的效用。然后，我们可以应用 `generate` 函数而不需要修改这个新语法。

```lisp
(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that)))

(setf *grammar* *bigger-grammar*)

> (generate 'sentence)
(A TABLE ON A TABLE IN THE BLUE ADIABATIC MAN SAW ROBIN
 WITH A LITTLE WOMAN)

> (generate 'sentence)
(TERRY SAW A ADIABATIC TABLE ON THE GREEN BALL BY THAT WITH KIM
 IN THESE BY A GREEN WOMAN BY A LITTLE ADIABATIC TABLE IN ROBIN
 ON LEE)

> (generate 'sentence)
(THE GREEN TABLE HIT IT WITH HE)
```
# 对多个程序使用相同的数据
用声明式形式表示信息的另一个优点是——作为规则或事实而不是Lisp函数——可以更容易地将信息用于多种目的。假设我们想要一个函数，它不仅能生成句子中的单词列表，还能生成句子完整语法的表示。例如，我们想要的不是列表（`a woman took a ball`），而是嵌套列表:
```lisp
(SENTENCE (NOUN-PHRASE (ARTICLE A) (NOUN WOMAN))
          (VERB-PHRASE (VERB TOOK)
                       (NOUN-PHRASE (ARTICLE A) (NOUN BALL))))
```
使用“直接函数（straightforward functions）”的方法，我们会陷入困境;我们必须重写每个函数来生成额外的结构。使用“（new notation）”方法，我们可以保持语法不变，只编写一个新函数:生成嵌套列表的一个版本。这两个变化是反对的类别到前面的每个重写，然后不附加在一起的结果，而只是列出他们与 `mapcar`：

```lisp
(defun generate-tree (phrase)
  "Generate a random sentence or phrase,
  with a complete parse tree."
  (cond ((listp phrase)
         (mapcar #'generate-tree phrase))
        ((rewrites phrase)
         (cons phrase
               (generate-tree (random-elt (rewrites phrase)))))
        (t (list phrase))))
```
```lisp
> (generate-tree 'Sentence)
(SENTENCE (NOUN-PHRASE (ARTICLE A)
                       (ADJ*)
                       (NOUN WOMAN)
                       (PP*))
      (VERB-PHRASE (VERB HIT)
                       (NOUN-PHRASE (PRONOUN HE))
                       (PP*)))

> (generate-tree 'Sentence)
(SENTENCE (NOUN-PHRASE (ARTICLE A)
                       (NOUN WOMAN))
          (VERB-PHRASE (VERB TOOK)
                       (NOUN-PHRASE (ARTICLE A) (NOUN BALL))))
```
作为单数据/多程序方法的另一个示例，我们可以开发一个函数来生成短语的所有可能的重写。 该函数 `generate-all` 返回一个短语列表，而不只是一个短语，并且我们定义了一个辅助函数 `combining-all`，用于管理结果的组合。 另外，有四种情况，而不是三种情况，因为我们必须显式检查 `nil`。 完整的程序仍然非常简单：
```lisp
(defun generate-all (phrase)
  "Generate a list of all possible expansions of this phrase."
  (cond ((null phrase) (list nil))
        ((listp phrase)
         (combine-all (generate-all (first phrase))
                      (generate-all (rest phrase))))
        ((rewrites phrase)
         (mappend #'generate-all (rewrites phrase)))
        (t (list (list phrase)))))

(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x.
  E.g., (combine-all '((a) (b)) '((1) (2)))
  -> ((A 1) (B 1) (A 2) (B 2))."
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (append x y)) xlist))
           ylist))
```
我们现在可以使用 `generate-all` 来测试我们原来的小语法。请注意，`generate-all` 的一个严重缺点是它不能处理 `*bigger-grammar*` 中出现的 `Adj* * => Adj + Adj*`之类的递归语法规则，因为这些规则会导致无限数量的输出。但是它对有限的语言很有效，比如由 `*simple-grammar*` 生成的语言:

```lisp
> (generate-all 'Article)

((THE) (A))

> (generate-all 'Noun)

((MAN) (BALL) (WOMAN) (TABLE))

> (generate-all 'noun-phrase)
((A MAN) (A BALL) (A WOMAN) (A TABLE)
 (THE MAN) (THE BALL) (THE WOMAN) (THE TABLE))

> (length (generate-all 'sentence))
256
```
它有 256 个句子，因为每个句子的形式都是：*Article-Noun-Verb-Article-Noun*，有两个冠词，四个名词和四个动词 `(2 x 4 x 4 x 2 x 4 = 256)`。

# 练习
- 描述 `combine-all` 的一种方法是计算附加在参数列表上的函数的叉乘。写出高阶函数的交叉积，并以此定义组合。
```lisp
(defun cross-product (fn xlist ylist)
  "Return a list of all (fn x y) values."
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (funcall fn x y))
                       xlist))
           ylist))

(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x"
  (cross-product #'append xlist ylist))
Now we can use the cross-product in other ways as well:

> (cross-product #'+ '(1 2 3) '(10 20 30))
(11 12 13
 21 22 23
 31 32 33)

> (cross-product #'list '(a b c d e f g h)
                        '(1 2 3 4 5 6 7 8))
((A 1) (B 1) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)
 (A 2) (B 2) (C 2) (D 2) (E 2) (F 2) (G 2) (H 2)
 (A 3) (B 3) (C 3) (D 3) (E 3) (F 3) (G 3) (H 3)
 (A 4) (B 4) (C 4) (D 4) (E 4) (F 4) (G 4) (H 4)
 (A 5) (B 5) (C 5) (D 5) (E 5) (F 5) (G 5) (H 5)
 (A 6) (B 6) (C 6) (D 6) (E 6) (F 6) (G 6) (H 6)
 (A 7) (B 7) (C 7) (D 7) (E 7) (F 7) (G 7) (H 7)
 (A 8) (B 8) (C 8) (D 8) (E 8) (F 8) (G 8) (H 8))
```

这里的寓意是让您的代码尽可能通用，因为您永远不知道接下来要用它做什么。
