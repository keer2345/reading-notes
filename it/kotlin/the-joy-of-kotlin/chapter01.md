**Chapter 01 [让程序更安全](https://livebook.manning.com/book/the-joy-of-kotlin/chapter-1）**

> brhhwfmq <> sharklasers.com 123456

本章概述：
- 识别程序陷阱
- 找出有副作用的问题
- 查找透明性使程序更安全
- 使用替代模型推理程序
- 充分利用抽象

本章简单讲述几个概念，比如不可变性、引用透明、替代模型，以及其他让你的程序更安全的建议，接下来的几章将反复应用这些感念。

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [程序陷阱](#程序陷阱)
    - [安全处理副作用](#安全处理副作用)
    - [通过引用透明性使程序更安全](#通过引用透明性使程序更安全)
- [程序安全的好处](#程序安全的好处)
    - [使用替代模型来推理程序](#使用替代模型来推理程序)
    - [将安全原则应用于简单示例](#将安全原则应用于简单示例)
    - [将抽象推向极限](#将抽象推向极限)

<!-- markdown-toc end -->


# 程序陷阱
- 避免引用（变量）的可变性，并将不可避免的突变情况抽象出来。
- 避免控制结构
- 将效果(与外部世界的交互)限制在代码的特定区域。这意味着不能打印到控制台或任何设备，也不能写入文件、数据库、网络或其他可能发生在这些受限区域之外的任何东西。
- 没有异常抛出。 抛出异常是分支的现代形式（GOTO），它导致产生所谓的意大利面条代码，这意味着您知道它的起始位置，但无法跟踪它的起始位置。 在第7章中，您将学习如何完全避免引发异常。


## 安全处理副作用

![](https://drek4537l1klr.cloudfront.net/saumont2/HighResolutionFigures/figure_1-1.png)

![](https://drek4537l1klr.cloudfront.net/saumont2/HighResolutionFigures/figure_1-2.png)

## 通过引用透明性使程序更安全

既不变异也不依赖外部世界的代码被称为参照透明的（referentially transparent）。 参照透明代码具有几个有趣的属性：
- 它是独立（self-contained）的。 您可以在任何上下文中使用它。 您要做的就是提供一个有效的参数。
- 
它是确定性的（deterministic）。 它总是为相同的参数返回相同的值，因此您不会感到惊讶。 但是，它可能返回错误的结果，但是至少对于相同的参数，结果永远不会改变。
- 它永远不会抛出任何异常。 它可能会引发错误，例如内存不足错误（OOME）或堆栈溢出错误（SOE），但是这些错误意味着代码中有错误。 无论是您（作为程序员）还是应该由您的API用户来处理（除了使应用程序崩溃（通常不会自动发生并最终修复该错误）之外）。
- 它不会创建导致其他代码意外失败的条件。 例如，它不会使参数或其他一些外部数据发生变异，从而使调用者发现其陈旧数据或并发访问异常。
- 它不依赖任何外部设备来工作。 它不会挂起，因为某些外部设备（无论是数据库，文件系统还是网络）不可用，速度太慢或损坏了。

![](https://drek4537l1klr.cloudfront.net/saumont2/HighResolutionFigures/figure_1-3.png)

# 程序安全的好处
- 更容易推理，因为它们是确定性的。
- 更易于测试。
- 更加模块化。
- 组合和重新组合要容易得多。
- 本质上是线程安全的，因为它们避免了共享状态的变化。

## 使用替代模型来推理程序

将参照透明表达式替换为其值不会改变整体含义：

![](https://drek4537l1klr.cloudfront.net/saumont2/HighResolutionFigures/figure_1-4.png)

```kotlin
fun main() {
  val x = add(mult(2, 3), mult(4, 5))
  println(x)
}

fun add(a: Int, b: Int) {
  log(String.format("Returning ${a + b} as the result of $a + $b"))
  return a + b
}

fun mult(a: Int, b: Int) = a * b

fun log(m: String) {
  println(m)
}
```

运行如下：
```kotlin
val x = add(6, 20)
```

##  将安全原则应用于简单示例
```kotlin
fun buyDonut(creditCard: CreditCard): Donut {
    val donut = Donut()
    creditCard.charge(donut.price)
    return donut
}
```

在此代码中，向信用卡收费是一个副作用。 为信用卡充值可能包括打电话给银行，验证信用卡是否有效和授权以及注册交易。 该函数返回甜甜圈。

这类代码的问题是很难测试。运行测试程序将涉及到联系银行并使用某种模拟帐户注册交易。或者您需要创建一个模拟信用卡来注册调用计费函数 `charge` 的效果，并在测试后验证模拟的状态。

如果您希望能够在不联系银行或不使用模拟程序的情况下测试程序，则应消除副作用。 但是，由于您仍想从信用卡中扣款，因此唯一的解决方案是将该操作的表示形式添加到返回值中。 您的 `buyDonut` 函数将必须同时返回甜甜圈和此付款方式。 要表示付款，可以使用 `Payment` 类，如下面所示：

```kotlin
class Payment(val creditCard: CreditCard, val amount: Int)
```

此类包含表示付款的必要数据，其中包括信用卡和收费金额。 因为 `buyDonut ` 函数必须同时返回一个 `Donut ` 和一个 `Payment`，所以您可以为此创建一个特定的类，例如 `Purchase`：

```kotlin
class Purchase(val donut: Donut, val payment: Payment)
```

您经常需要一个此类来容纳两个（或多个）不同类型的值，因为为了使程序更安全，您必须用返回这些效果的表示形式来替换副作用。

除了创建特定的 `Purchase` 类，您可以使用通用的 `Pair`。 此类由其包含的两种类型（在本例中为 `Donut` 和 `Payment`）进行参数化。Kotlin 提供了该类以及 `Triple`，该类允许表示三个值。 这样的类在 Java 之类的语言中将很有用，因为定义 `Purchase` 类将意味着编写构造函数，`getters`（可能）以及 `equals` 和 `hashcode` 方法以及 `toString`。 在 Kotlin 中，这用处不大了，因为只需一行代码即可获得相同的结果：
```kotlin
data class Purchase(val donut: Donut, val payment: Payment)
```

`Purchase` 类已经不需要显式的构造函数和 `getter`。 通过在类定义的前面添加 `data` 关键字，Kotlin 还提供了 `equals`，`hashCode`，`toString` 和 `copy` 的实现。 但是您必须接受默认的实现。 如果所有属性均相等，则数据类的两个实例将相等。 如果这不是您所需要的，则可以使用自己的实现覆盖这些功能中的任何一个。

```kotlin
fun buyDonut(creditCard: CreditCard): Purchase {
    val donut = Donut()
    val payment = Payment(creditCard, Donut.price)
    return Purchase(donut, payment)
}
```

将多个付款组合成一个付款：

```kotlin
package com.fpinkotlin.introduction.listing03
class Payment(val creditCard: CreditCard, val amount: Int) {
    fun combine(payment: Payment): Payment =
        if (creditCard == payment.creditCard)
            Payment(creditCard, amount + payment.amount)
        else
            throw IllegalStateException("Cards don't match.")
}
```


一次性购买多个甜甜圈：


```kotlin
package com.fpinkotlin.introduction.listing05

data class Purchase(val donuts: List<Donut>, val payment: Payment)

fun buyDonuts(quantity: Int = 1, creditCard: CreditCard): Purchase =
        Purchase(List(quantity) {
            Donut()
        }, Payment(creditCard, Donut.price * quantity))
```

函数 `{ Donut() }` 等价于：
```kotlin
{ index -> Donut{} }
```
或者：
```kotlin
{ _ -> Donut{} }
```

函数 `buyDonuts` 的参数 `quantity` 默认值为 `1`，我们可以不指定 `quantity` 来调用 `buyDonuts`：
```kotlin
buyDonuts(creditCard = c)
```

在 Java 中我们必须要使用第二种实现重载方法，比如：
```java
public static Purchase buyDonuts(CreditCard creditCard) {
    return buyDonuts(1, creditCard);
}

public static Purchase buyDonuts(int quantity,
                                 CreditCard creditCard) {
    return new Purchase(Collections.nCopies(quantity, new Donut()),
                        new Payment(creditCard, Donut.price * quantity));
}
```

现在，可以测试代码：
```kotlin
import org.junit.Assert.assertEquals
import org.junit.Test

class DonutShopKtTest {
    @Test
    fun testBuyDonuts() {
        val creditCard = CreditCard()
        val purchase = buyDonuts(5, creditCard)
        assertEquals(Donut.price * 5, purchase.payment.amount)
        assertEquals(creditCard, purchase.payment.creditCard)
    }
}
```
重构代码的另一个好处是程序更容易组合。 如果同一个人在您的初始计划中进行了多次购买，则每次该人购买商品时，您都必须与银行联系（并支付相应的费用）。 但是，在新版本中，您可以选择为每次购买立即对卡进行收费，或将使用同一张卡进行的所有付款归为一组，并且只对一次总费用进行收费。 要对付款进行分组，您需要使用 Kotlin List 类中的其他功能：
- `groupBy(f: (A) -> B): Map<B, List<A>>`
- `values: List<A>`
- `map(f: (A) -> B): List<B>`
- `reduce(f: (A, A) -> A): A`

使用这些函数，您现在可以创建一个新函数，按信用卡对付款进行分组，如下面的清单所示：

```kotlin
package com.fpinkotlin.introduction.listing05;

class Payment(val creditCard: CreditCard, val amount: Int) {
    fun combine(payment: Payment): Payment =
        if (creditCard == payment.creditCard)
            Payment(creditCard, amount + payment.amount)
        else
            throw IllegalStateException("Cards don't match.")
    companion object {
        fun groupByCard(payments: List<Payment>): List<Payment> =
            payments.groupBy { it.creditCard }    
                    .values    

                         .map { it.reduce(Payment::combine) }    

    }
}
```

## 将抽象推向极限
将抽象推到极限可以使程序更安全，因为抽象的部分将只编写一次。 因此，一旦对其进行了全面的测试，就不会有通过重新实现而产生新错误的风险。
