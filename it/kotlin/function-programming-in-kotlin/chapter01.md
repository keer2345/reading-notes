**Chapter 01 [What is functional programming?](https://livebook.manning.com/book/functional-programming-in-kotlin/chapter-1/v-8/1)**

> sjwngqds <> sharklasers.com 123456


本章涵盖：
- 副作用及其带来的问题
- 通过函数解决方案消除副作用
- 定义纯函数
- 用替代模型证明参照的透明度和纯度

本章会看到带有副作用代码块的简单例子一步步重构成函数的形式。函数式编程其中一个核心概念是消除副作用，也是本章的重点之一。

函数式编程严重依赖于类似Kotlin等静态类型化语言提供的类型系统，通常称为类型化函数编程（typed functional programming）。我们还会提到范畴论（category theory），它是数学的一个分支，与这种编程风格非常紧密地联系在一起。由于这种数学上的倾向性，要对代数、证明和定律等词汇有所准备。

函数式的副作用在：
- 在发生更改的块范围之外修改变量
- 修改数据结构
- 设置对象的字段域
- 抛出异常或因为错误而终止
- 打印到终端或读取用户输入
- 读写文件
- 屏幕上绘图


# 函数式编程的好处
我们先来看个例子，可以先不用关注它的语法。

> 当然，本书是关于 Kotlin 和 FP 的，我们假设你对 Kotlin 有了一定的了解。比如阅读了 *Kotlin in Action* 一书。


## 一段有副作用的程序
``` kotlin
class Cafe {
    fun buyCoffee(cc: CreditCard): Coffee {
        val cup = Coffee()
        cc.charge(cup.price)
        return cup
    }
}
```

`cc.charge(cup.price)` 是存在副作用的。支付调用了外部的交互——假设它需要通过某些Web服务与信用卡提供商联系，授权交易，向卡收费，并且（如果成功的话）保存交易记录以供以后参考。然而我们的函数仅仅是返回一杯 `Coffee`，其他那些都可以放在一边，因此他们是有“副作用”的。

正因为这些副作用，代码很难测试。我们并不想去连接信用卡服务商和支付费用。`CreditCard` 不应该包含任何与实际信用卡供应商有联系的收费，也不应该保存收费记录在我们的内部系统。我们可以让代码模块化和易于测试，让 `CreditCard` 不知道这些问题，通过 `Payments` 对象传递给 `buyCoffee`。

``` kotlin
class Cafe {
    fun buyCoffee(cc: CreditCard, p: Payments): Coffee {
            val cup = Coffee()
            p.charge(cc, cup.price)
            return cup
        }
}
```
在调用 `p.charge(cc, cup.price)` 虽然还是有副作用，不过至少而已测试了。Payments 可以有个接口，我们可以编写 mock 实现该接口用于测试。但仍旧不够好，当一个具体的类在其他方面可能还不错的时候，我们被迫在接口上付款，而任何模拟实现都将难以使用。例如，它可能包含了一些内部状态，在调用 `buyCoffee` 之后我们必须检查这些状态。并且，我们的测试必须确保这个状态通过调用 `charge` 来修改。我们使用 mock 框架或者类似的框架来处理细节，但如果只是想测试 `buyCoffee` 就显得太夸张了。

除了测试，另一个问题是 `buyCoffee` 难以重复使用。设想一个顾客，Alice，要下单 12 杯咖啡，理想情况下我们可以重复使用 `buyCoffee`，可能是调用 12 次循环。但就目前的执行情况而言，这将涉及联系支付提供商12次，授权12个单独的费用到爱丽丝的信用卡!这增加了更多的加工费，对 Alice 和咖啡店来说都不是好事。

我们需要些个函数 `buyCoffee`，带有汇总费用的逻辑。

## 函数解决移除副作用
函数解决方案就是消除副作用，`buyCoffee` 返回消费值，除了返回 `Coffee` 还返回消费值。通过将收费发送给信用卡供应商、持久化收费记录等方式处理收费的问题将在其他地方处理。

![](https://drek4537l1klr.cloudfront.net/vermeulen/v-8/Figures/buycoffee_with_and_without_side_effect.png)


函数看起来像这样：
``` kotlin
class Cafe {
        fun buyCoffee(cc: CreditCard): Pair<Coffee, Charge> {
            val cup = Coffee()
            return Pair(cup, Charge(cc, cup.price))
        }
    }
```

这里将创建支付分离开来。我们将看到很容易重用它来在一个事务购买多样咖啡。`Charge` 是包含 `CreditCard` 和 `amount` 的数据类型：

``` kotlin
data class Charge(val cc: CreditCard, val amount: Float) {

        fun combine(other: Charge): Charge =
            if (cc == other.cc)
                Charge(cc, amount + other.amount)
            else throw Exception(
                "Cannot combine charges to different cards"
            )
    }
```

现在，我们来看一下如何实现 `n` 杯咖啡的购买：
``` kotlin
class Cafe {

        fun buyCoffee(cc: CreditCard): Pair<Coffee, Charge> = TODO()

        fun buyCoffees(
            cc: CreditCard,
            n: Int
        ): Pair<List<Coffee>, Charge> {

            val purchases: List<Pair<Coffee, Charge>> =
                List(n) { buyCoffee(cc) }

            val (coffees, charges) = purchases.unzip()

            return Pair(
                coffees,
                charges.reduce { c1, c2 -> c1.combine(c2) }
            )
        }
    }
```

**数据的解构：**
```kotlin
val (left, right) = Pair(1, 2)
assert left == 1
assert right == 2

val (_, right) = Pair(1, 2)
```

# 明确什么是纯函数
![](https://drek4537l1klr.cloudfront.net/vermeulen/v-8/Figures/pure_function.png)

# 参考透明度、纯度和替代模型

![](https://drek4537l1klr.cloudfront.net/vermeulen/v-8/Figures/referentially_transparent.png)

![](https://drek4537l1klr.cloudfront.net/vermeulen/v-8/Figures/side_effect.png)
