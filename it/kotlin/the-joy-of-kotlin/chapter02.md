# 
**Chapter 02 [Functional programming in Kotlin: An overview](https://livebook.manning.com/book/the-joy-of-kotlin/chapter-2)**

> brhhwfmk <> sharklasers.com 123456

本章涵盖：
- 声明和初始化字段、变量
- Kotlin 的类和接口
- 集合的两种类型
- 函数（和控制结构）
- 处理 null

# 字段和变量
```kotlin
val name: String = "Mickey"
```
## 为了简单可省略类型

```kotlin
val name = "Mickey"
```
## 可变字段
```kotlin
var name = "Mickey"
name = "Donald"
```
## 了解延迟初始化
```kotlin
var name: String? = null

// ...

name = getName()
```
`String?` 表示可为空值。我们也可以先初始化一个值：
```kotlin
var name: String = "NOT_INITIALIZED_YET"

// ...

name = getValue()
```
或者，如果名称永远不应该为空，则可以使用空字符串来表示未初始化的引用。在任何情况下，即使初始化后的值从未更改，也必须使用 `var`。但是 Kotlin 提供了一个更好的解决方案：
```kotlin
val name: String by lazy { getName() }
```
这样，当 `name` 第一次被引用时 `getName()` 函数将仅被调用一次。我们也可以用 lambda 来替代函数引用：

```kotlin
val name: String by lazy(::getName)
```
说 `name` 何时被首次引用是指它什么时候被取消引用，以便于它指向的值能被使用。看下面的例子：
```kotlin
fun main(args: Array<String>) {

    val name: String by lazy { getName() }
    println("hey1")
    val name2: String by lazy { name }
    println("hey2")

    println(name)
    println(name2)
    println(name)
    println(name2)
}

fun getName(): String {
    println("computing name...")
    return "Mickey"
}
```
结果如下：
```
hey1
hey2
computing name...
Mickey
Mickey
Mickey
Mickey
```
延迟初始化不能用于可变引用。如果您确实需要一个延迟的可变引用，您可以使用 `lateinit` 关键字，这在某种程度上具有相同的效果，但不需要自动按需初始化
```kotlin
lateinit var name: String

// ...

name = getName()
```
这种构造避免使用可为空的类型。 但是，与延迟相比，它绝对没有任何好处，除非应该在外部进行初始化，例如在使用属性时使用依赖项注入框架。 请注意，始终应该首选基于构造函数的依赖注入，因为它允许使用不可变的属性。 正如您将在第 9 章中看到的那样，关于懒惰的知识还有很多。

# Kotlin的类和接口

```kotlin
class Person constructor(name: String) {

    val name: String

    init {
        this.name = name
    }
}
```
- Kotlin 类默认是 public 的，所以不需要关键字 `public`。非 public 的类可以使用关键字 `private`, `protected`, `internal`，`internal` 表示该类只能在模块的内部访问。
- Kotlin 类默认是 final 的，想让它可以被继承，需要加上 `open`。
- 构造器声明在类名之后，具体的实现在 `init` 块。
- 访问器不是必须的，它会在你编译代码的时候生成。
- 不像 Java，公共类不需要在具有类名的文件中定义，可以用你喜欢的名字命名文件。而且，你可以在同一个文件中定义几个公共类，但并不意味着应该这么做，将每个公共类放在一个单独的文件中，使用类的名称可以使查找更加容易。

## 让代码更加简洁
简化 1：
```kotlin
class Person constructor(name: String) {

    val name: String = name
}
```
简化 2：
```kotlin
class Person constructor(val name: String) {

}
```
简化 3：
```kotlin
class Person (val name: String)
```
另外，你可以创建多个属性：
```kotlin
class Person(val name: String, val registered: Instant)
```

## 实现接口和扩展类 
如果想让类实现一个或多个接口，或者从另一个类扩展，可以在类声明之后列出它们：

```kotlin
class Person(val name: String,
             val registered: Instant) : Serializable, Comparable<Person> {
    override fun compareTo(other: Person): Int {
        ...
    }
}
```
使用同样的语法扩展类，不同之处是扩展类的名称后面跟随括号及其参数：

```kotlin
class Member(name: String, registered: Instant) : Person(name, registered)
```
记住，类默认是 `final` 的，被扩展的类必须声明为 `open`：

```kotlin
open class Person(val name: String, val registered: Instant)
```
良好的编程习惯是仅允许对其专门设计的类进行扩展。 如您所见，与 Java 不同，Kotlin 试图通过不让您扩展未设计为扩展的类的方式来强制执行此原则。

## 初始化类
当创建了一个类的实例，Kotlin 使您免于重复键入，尽管程度较小。 例如，代替写作：
```kotlin
final Person person = new Person("Bob", Instant.now());
```
你可以将构造器作为函数使用（事实上的确如此）：
```kotlin
val person = Person("Bob", Instant.now())
```

## 重载构造器的属性


有时，属性是可选的，并且具有默认值。 在上一个示例中，您可以确定注册日期默认为实例创建日期。 在Java中，您必须编写以下清单中所示的两个构造函数：
```java
public final class Person {

    private final String name;

    private final Instant registered;

    public Person(String name, Instant registered) {
        this.name = name;
        this.registered = registered;
    }

    public Person(String name) {
        this(name, Instant.now());
    }

    public String getName() {
        return name;
    }

    public Instant getRegistered() {
        return registered;
    }
}
```
在 Kotlin，可以通过默认值来达到同样的结果：

```kotlin
class Person(val name: String, val registered: Instant = Instant.now())
```
还可以通过更多传统的方式重写构造函数：
```kotlin
class Person(val name: String, val registered: Instant = Instant.now()) {
	constructor(name: Name) : this(name.toString()) {
		// optional constructor implementation may be added
	}
}
```
私有化构造器和属性：

```kotlin
class Person private constructor(val name: String)
```

访问器和属性

```kotlin
val person = Person("Bob")

// ...

println(person.name)    // Calling the getter
```
## 创建equals和hashCode方法
在 Java 中通过 Intellij 生成这样的代码：
```java
public final class Person {

    private final String name;

    private final Instant registered;

    public Person(String name, Instant registered) {
        this.name = name;
        this.registered = registered;
    }

    public Person(String name) {
        this(name, Instant.now());
    }

    public String getName() {
        return name;
    }

    public Instant getRegistered() {
        return registered;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Person person = (Person) o;
        return Objects.equals(name, person.name) &&
                Objects.equals(registered, person.registered);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, registered);
    }
}
```
而 Kotlin 就很简单了：
```kotlin
data class Person(val name: String, val registered: Instant = Instant.now())
```
## 解构数据对象
在每个数据类型中有 n 个属性，函数 `component1` 到 `componentN` 是自动定义的，这样一来，您就可以按在类中定义的顺序访问属性。 此功能的主要用途是解构对象，这使对它们的属性的访问更加简单：
```kotlin
data class Person(val name: String, val registered: Instant = Instant.now())

fun show(persons: List<Person>) {
    for ((name, date) in persons)
        println(name + "'s registration date: " + date)
}

fun main(args: Array<String>) {
    val persons = listOf(Person("Mike"), Person("Paul"))
    show(persons)
}
```
`show` 函数等价于：
```kotlin
fun show(persons: List<Person>) {
    for (person in persons)
        println(person.component1()
            + "'s registration date: " + person.component2())
}
```
如您所见，通过避免每次使用对象属性时取消对对象属性的引用，解构可以使代码更清晰，更省力。

## 在Kotlin中实现静态成员
Kotlin 的类没有静态成员，为了达到同样的效果，你可以使用称为**伴生对象**（companion object）的特殊结构：

```kotlin
data class Person(val name: String,
                  val registered: Instant = Instant.now()) {

    companion object {
        fun create(xml: String): Person {
            TODO("Write an implementation creating " +
                 "a Person from an xml string")
        }
    }
}
```
可以像类似 Java 中的静态方法那样调用 `create` 函数：
```kotlin
Person.create(someXmlString)
```
顺便说一下，您可以看到Kotlin提供了一个TODO函数，这使您的代码更加一致。此方法在运行时抛出异常，提醒您应该完成的工作

## 使用单例
在 Kotlin，单例可以很容易的通过在类名前面添加关键词 *object* 来实现：

```kotlin
object MyWindowAdapter: WindowAdapter() {
    override fun windowClosed(e: WindowEvent?) {
        TODO("not implemented")
    }
}
```
对象不能有构造函数。如果它有属性，这些属性必须初始化或抽象。

## 防止实用类的实例化

```kotlin
package com.acme.util

fun create(xml: String): Person {
  ...
}
```
调用：
```kotlin
val person = com.acme.util.create(someXmlString)
```
或者这样调用：
```kotlin
import com.acme.util.*

val person = create(someXmlString)
```
## Kotlin没有基元
## Kotlin的两种集合

```kotlin
val list = listOf(1, 2, 3)
```

```kotlin
val list1 = listOf(1, 2, 3)
val list2 = list1 + 4
val list3 = list1 + list2
println(list1)
println(list2)
println(list3)
```

结果如下：
```
[1, 2, 3] 
[1, 2, 3, 4] 
[1, 2, 3, 1, 2, 3, 4]
```
可变集合：
```kotlin
val list1 = mutableListOf(1, 2, 3)
val list2 = list1.add(4)
val list3 = list1.addAll(list1)
println(list1)
println(list2)
println(list3)
```
结果如下：
```
[1, 2, 3, 4, 1, 2, 3, 4]
true
true
```
# Kotlin的包
# Kotlin可视化
- Maven project
- Gradle source set
- Intellij module
- Eclipse project
- a group of files compiled with a single Ant task


默认情况下，类和接口具有公共可见性。 它们还可能受到以下可见性修改器之一的影响：
- `private`
- `projected`
- `internal`
- `public`

# 函数
# 函数声明

```kotlin
fun add(a: Int, b: Int): Int {
  return a + b
}
```

```kotlin
fun add(a: Int, b: Int): Int = a + b
```

```kotlin
fun add(a: Int, b: Int) = a + b
```
请注意，将两种语法混合使用可能会产生意想不到的结果。 以下示例完全有效：

```kotlin
fun add(a: Int, b: Int) = {
    a + b
}
```
## 本地函数声明
```kotlin
fun sumOfPrimes(limit: Int): Long {

    val seq: Sequence<Long> = sequenceOf(2L) +
            generateSequence(3L, {
                it + 2
            }).takeWhile{
                it < limit
            }

    fun isPrime(n: Long): Boolean =
            seq.takeWhile {
                it * it <= n
            }.all {
                n % it != 0L
            }

    return seq.filter(::isPrime).sum()
}
```
```kotlin
```
```kotlin
```
```kotlin
```
```kotlin
```
```kotlin
```kotlin
```kotlin
```kotlin
```kotlin
```kotlin
```kotlin
```kotlin
```kotlin
```kotlin
