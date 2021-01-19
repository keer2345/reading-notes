**Chapter 02 [Getting started with functional programming in Kotlin](https://livebook.manning.com/book/functional-programming-in-kotlin/chapter-2/v-8/1)**

> sjwngqds <> sharklasers.com 123456

本章涵盖：
- 高阶函数
- 递归
- 抽象高阶函数编程多态
- 通过匿名函数调用高阶函数
- 实现多态函数


本章我们通过组合存函数来学习在 Kotlin 中如何编程，介绍基本的函数式编程，并讨论如何使用尾部递归编写循环，以及介绍高阶函数。

# 高阶函数：将函数传递给函数

首先要知道：函数也是值，就像其他类型——例如整数、字符串、列表——函数可以分配给变量，存储在数据结构中，并作为参数传递给函数。


当我们编写纯函数程序时，会经常编写带有函数作为参数的函数，这种函数成为高阶函数。但是在刚开始，假设我们想要让程序适用于打印数值的绝对值以及阶乘，类似这样：
```kotlin
The absolute value of -42 is 42
The factorial of 7 is 5040
```


## 简短介绍：函数式编写循环

我们增加新的函数来计算第 *n* 个数的阶乘，通过递归来实现。

首先编写 `factorial`:
```kotlin
fun factorial(i: Int): Int {
  fun go(n: Int, acc: Int): Int = if (n <= 0) acc else go(n-1, n * acc)
  return go(i, 1)
}
```
Kotlin 不手动探测这种递归，但要求函数声明 `tailrec` 修饰语：
```kotlin
fun factorial(i: Int): Int {
  tailrec fun go(n: Int, acc: Int): Int = if (n <= 0) acc else go(n-1, n * acc)
  return go(i, 1)
}
```
如果递归函数在尾部位置有一个调用，但没有将自己声明为 `tailrec`，那么编译器不会消除尾部调用，反而可能会导致 `StackOverflowError` 异常。

在对函数应用tailrec修饰符而其最终声明不处于尾部位置的情况下，编译器将报错：
```kotlin
Warning:(19, 9) Kotlin: A function is marked as tail-recursive but no tail calls are found
```
尽管发出警告总比没有强，但在这种情况下，出现编译错误会更有帮助，也更安全。


**练习 2.1**

编写一个递归函数获取斐波纳契（Fibonacci）数列的第 n 个数：前面两个数是 0 和 1，第 n 个数是它前面两个之和，比如 0，1，1，2，3，5，8，13，21……
```kotlin
fun fibonacci(i:Int):Int {
	tailrec fun go(n: Int, prev: Int, cur: Int):Int =
    	if (n<=0) prev
    	else go(n-1, cur, prev + cur)
    return go(i, 0, 1)
}
```
## 编写我们的第一个高阶函数

![](https://drek4537l1klr.cloudfront.net/vermeulen/v-9/Figures/new_code_branch.png)

这是个简单的阶乘函数：
```kotlin
object Example {
    private fun abs(n:Int):Int = 
    if (n<0) -n
	else n

    private fun factorial(i:Int):Int {
        fun go(n:Int,acc:Int):Int = 
            if (n<=0) acc
            else go(n-1, n * acc)
        return go(i, 1)
    }

    fun formatAbs(x: Int): String {
        val msg = "The absolute value of %d is %d"
        return msg.format(x, abs(x))
    }
    
    fun formatFactorial(x: Int): String {
        val msg = "The factorial value of %d is %d"
        return msg.format(x, factorial(x))
    }
}

fun main() {
    println(Example.formatAbs(-42))
    println(Example.formatFactorial(7))
}
```
结果如下：   
```
The absolute value of -42 is 42
The factorial value of 7 is 5040
```

两个函数 `formatAbs` 和 `formatFactorial` 很相似，我们可以将他们归纳为一个函数 `formatResult`，它接受一个函数作为入参：
```kotlin
object Example {
    fun abs(n:Int):Int = 
        if (n<0) -n
        else n

    fun factorial(i:Int):Int {
        fun go(n:Int,acc:Int):Int = 
            if (n<=0) acc
            else go(n-1, n * acc)
        return go(i, 1)
    }
 
    fun formatResult(name: String, n: Int, f:(Int) -> Int):String {
        val msg = "The %s value of %d is %d"
        return msg.format(name, n, f(n))
    }
}


fun main() { 
    println(Example.formatResult("absolute", -42, Example::abs))
    println(Example.formatResult("factorial", 7, Example::factorial))
}
```
函数作为入参的引用形式：`::abs` 等价于 `this::abs`，在范围外的话就通过完全引用：`Example::abs`，或者导入命名空间引用：
```kotlin
import Example.factorial

// ...

formatResult("factorial", 7, ::factorial)
```

我们还可以通过字面函数（又称为 *anonymous function* 或 *lambda*）作为参数：
```kotlin
formatResult("absolute",
             -42,
             fun(n: Int): Int { return if (n < 0) -n else n }
)
```
这样看起来似乎有点笨拙，可以更加简化一些：
```kotlin
formatResult("absolute", -42, { n -> if (n < 0) -n else n })
```
如果 lambda 函数只有**一个**参数，可以通过隐式便利参数（implicit convenience parameter）`it` 来替换：
```kotlin
formatResult("absolute", -42, { if (it < 0) -it else it })
```

# 多态函数：对类型抽象
目前我们定义的只是单态（monomorphic）函数，函数只是操作一种类型的数据。例如 `abs` 和 `factorial` 指定参数类型为 `Int`，以及高阶函数 `formatResult` 也是固定为以 `Int` 类型作为入参的函数来操作。通常，特别是写高阶函数时，我们想让代码能供任何类型工作，这种就称为多态（polymorphic）函数。

## 多态函数举例
我们经常可以通过观察几个单态函数都具有相似的结构来发现多态函数。例如下面的单态函数 `findFirst`，返回数组的第一个索引，或者返回 `-1`(如果没有找到的话)。它专门用于在字符串的数组中查一个字符串：
```kotlin
fun findFirst(ss: Array<String>, key: String): Int {
    tailrec fun loop(n: Int): Int =
    	when {
            n >= ss.size -> -1
            ss[n] == key -> n
            else -> loop(n + 1)
        }

	return loop(0)
}


fun main() {  
    println(findFirst(arrayOf("3","4","5","6"), "5"))
}
```
这里先不讨论代码的详细实现，重点是我们如何通过 `findFirst` 在 `Array<String>` 搜索 `String`，在 `Array<Int>` 搜索 `Int`，或者在 `Array<A>` 搜索指定的 `A` 类型。：
```kotlin
fun findFirst(ss: Array<String>, key: String): Int {
    tailrec fun loop(n: Int): Int =
    	when {
            n >= ss.size -> -1
            ss[n] == key -> n
            else -> loop(n + 1)
        }
	return loop(0)
}

fun <A> findFirst(xs: Array<A>, p: (A) -> Boolean): Int {
    tailrec fun loop(n: Int): Int =
    	when {
            n >= xs.size -> -1
            p(xs[n]) -> n
            else -> loop(n + 1)
        }
        
     return loop(0)
}

fun main() {  
    println(findFirst(arrayOf("3","4","5","6"), "5"))
    println(findFirst<Int>(arrayOf(4,5,6,7), {it == 7}))
}
```

这是个多态函数的例子，将数组的类型和函数抽象化用于查找。编写一个多态函数，通常在函数名前面加上类似 `<A>`，也可以将参数类型定义为我们想要的——`<Foo, Bar, Baz`，或者 `<TheParameter, another_good_one>` 等合法类型的声明，按照惯例，我们一般使用简短的单个大写字母，类似 `<A, B, C>`。


函数 `findFirst` 的 `A` 在两个地方引用：`Array<A>` 和 `p:(A) -> Boolean`，因此这两处必须是相同的类型 `A`，如果我们尝试在 `Array<Int>` 中查找 `String` 就会报错。

**练习 2.2**
实现 `isSorted`，校验是否是已排序的列表。`List<A>` 根据给定的比较函数来排序，该函数之前先添加两个 `List` 的 `head` 和 `tail` 两个扩展属性：
```kotlin
val <T> List<T>.tail: List<T> 
    get() = drop(1)

val <T> List<T>.head: T
    get() = first()
    
fun <A> isSorted(aa: List<A>, order: (A, A) -> Boolean): Boolean {
    tailrec fun loop(n:Int):Boolean = 
    	when {
            n >= aa.size - 1 -> true
            order(aa[n+1], aa[n]) -> false
            else -> loop(n+1)
        }
    
    return loop(0)
}

fun main() {   
    println(isSorted(listOf(1,2,3,4)) { a,b -> b > a})
    println(isSorted(listOf(1,3,2,4)) { a,b -> b > a})
    println(isSorted(listOf("a","b","c","d")) { a,b -> b < a})
}
```

**扩展方法和属性**

我们可以很容易将行为通过扩展方法的方式添加到给定类型的所有实例中：
```kotlin
fun Int.show(): String = "The value of this Int is $this"
```
结果：
```kotlin
>>> 1.show()
res1: kotlin.String = The value of this Int is 1
```

类似地，我们可以公开所有实例的属性：
```kotlin
val Int.show: String
    get() = "The value of this Int is $this"
```

结果：
```kotlin
>>> 1.show()
res2: kotlin.String = The value of this Int is 1
```

这些扩展方法和属性是静态分派的，换句话说，我们实际上并没有修改底层类。被调用的扩展函数由调用该函数的表达式的类型决定，而不是由运行时对该表达式求值的结果的类型决定。

## 通过匿名函数调用高阶函数
使用高阶函数时，使用匿名函数调用时很方便的，例如前面说到的 `findFirst`：
```kotlin
findFirst(arrayOf(7, 9, 13), { i: Int -> i == 9 })
```
# 类型的实现
偏函数：
```kotlin
fun <A, B, C> partial1(a: A, f: (A, B) -> C): (B) -> C = TODO()
```
```kotlin
fun <A, B, C> partial1(a: A, f: (A, B) -> C): (B) -> C =
        { b: B -> TODO() }
```
```kotlin
fun <A, B, C> partial1(a: A, f: (A, B) -> C): (B) -> C =
        { b: B -> f(a, b) }
```

**练习2.3**
```kotlin
fun <A, B, C> curry(f: (A, B) -> C): (A) -> (B) -> C = TODO()
```

**练习2.4**
```kotlin
fun <A, B, C> curry(f: (A, B) -> C): (A) -> (B) -> C = TODO()
```

**练习2.5**
```kotlin
fun <A, B, C> compose(f: (B) -> C, g: (A) -> B): (A) -> C = TODO()
```
