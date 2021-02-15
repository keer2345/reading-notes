**[Your way to Groovy](https://livebook.manning.com/book/groovy-in-action-second-edition/chapter-1)

> groovy01 <> sharklasers.com

This chapter covers
- What Groovy is all about
- How it makes your programming life easier
- How to start

This first chapter provides background information about Groovy and everything you need to know to get started. It starts with the Groovy story: why Groovy was created, what considerations drive its design, and how it positions itself in the landscape of languages and technologies. The next section expands on Groovy’s merits and how they can make life easier for you, whether you’re a Java programmer, a script aficionado, or an agile developer. 

- [The Groovy Story](#the-groovy-story)
  - [What is Groovy?](#what-is-groovy)
  - [Playing nicely with Java: seamless integration](#playing-nicely-with-java-seamless-integration)
  - [Power in your code: a feature-rich language](#power-in-your-code-a-feature-rich-language)
  - [Community driven but corporate backed](#community-driven-but-corporate-backed)
- [What Groovy can do for you](#what-groovy-can-do-for-you)
- [Running Groovy](#running-groovy)
  - [Using groovysh for a welcome message](#using-groovysh-for-a-welcome-message)
  - [Using groovyConsole](#using-groovyconsole)
  - [Using the groovy command](#using-the-groovy-command)
- [Compiling and running Groovy](#compiling-and-running-groovy)
  - [Compiling Groovy with groovyc](#compiling-groovy-with-groovyc)
  - [Running a compiled Groovy script with Java](#running-a-compiled-groovy-script-with-java)
- [Groovy IDE and editor support](#groovy-ide-and-editor-support)
- [Summary](#summary)

# The Groovy Story

The landscape of JVM-based languages. Groovy is a feature-rich and Java-friendly language—it excels at both sides instead of sacrificing one for the sake of the other. 

![](https://drek4537l1klr.cloudfront.net/koenig2/Figures/01fig01.jpg)

## What is Groovy? 

Groovy is an optionally typed, dynamic language for the Java platform with many features that are inspired by languages like Python, Ruby, and Smalltalk, making them available to Java developers using a Java-like syntax. Unlike other alternative languages, it’s designed as a companion to, not a replacement for, Java. 

## Playing nicely with Java: seamless integration

Groovy and Java join together in a tongue-and-groove fashion. 

![](https://drek4537l1klr.cloudfront.net/koenig2/Figures/01fig02.jpg)

The integration ladder shows increasing cross-language support from simple calls for interoperability up to seamless tool integration. 

![](https://drek4537l1klr.cloudfront.net/koenig2/Figures/01fig03.jpg)

True seamless integration means that you can take any Java class from a given Java codebase and replace it with a Groovy class. Likewise, you can take any Groovy class and rewrite it in Java, both without touching any other class in the code base. That’s what we call a drop-in replacement, which imposes further considerations about annotations, static members, and accessibility of the used libraries from Java. 

Syntax alignment

The second dimension of Groovy’s friendliness is its syntax alignment. Let’s compare the different mechanisms to obtain today’s date in various languages to demonstrate what alignment should mean:

```
import java.util.*;       // Java
Date today = new Date();  // Java

today = new Date()        // Groovy

require 'date'             # Ruby
today = Date.new           # Ruby

import java.util._        // Scala
var today = new Date      // Scala

(import '(java.util Date)) ; Clojure
(def today (new Date))     ; Clojure
(def today (Date.))        ; Clojure alternative
```

## Power in your code: a feature-rich language
Many of the additional libraries and GDK enhancements in Groovy build on the new language features. The combination of the three forms a “sweet spot” for clear and powerful code. 

![](https://drek4537l1klr.cloudfront.net/koenig2/Figures/01fig04_alt.jpg)

Unfortunately, many of the features can’t be understood in just a few words. **Closures**, for example, are an invaluable language concept in Groovy, but the word on its own doesn’t tell you anything. We won’t go into all the details now, but here are a few examples to whet your appetite. 

**Listing a file: closures and I/O additions**

Closures are blocks of code that can be treated as first-class objects: passed around as references, stored, executed at arbitrary times, and so on. Java’s anonymous inner classes are often used this way, particularly with adapter classes, but the syntax of inner classes is ugly, and they’re limited in terms of the data they can access and change. 

ile handling in Groovy is made significantly easier with the addition of various methods to classes in the java.io package. A great example is the File.eachLine method. How often have you needed to read a file, a line at a time, and perform the same action on each line, closing the file at the end? This is such a common task; it shouldn’t be difficult. In Groovy, it isn’t. 
```groovy
def number = 0
new File('data.txt').eachLine { line ->
    number++
    println "$number: $line"
}
```
Result:
```
1: first line
2: second line
```

**Printing a list: collection literals and simplified property access**
```groovy
def classes = [String, List, File]
for (clazz in classes) {
    println clazz.package.name
}
```
```
java.lang
java.util
java.io
```


```groovy
println( [String, List, File]*.package*.name )
```
```
[java.lang, java.util, java.io]
```


**XML handling the Groovy way: GPath with dynamic properties**
`customers.xml`:
```xml
<?xml version="1.0" ?>
<customers>
  <corporate>
    <customer name="Bill Gates"      company="Microsoft" />
    <customer name="Tim Cook"        company="Apple"     />
    <customer name="Larry Ellison"   company="Oracle"    />
  </corporate>
  <consumer>
    <customer name="John Doe" />
    <customer name="Jane Doe" />
  </consumer>
</customers>
```
```groovy
def customers = new XmlSlurper().parse(new File('customers.xml'))
for (customer in customers.corporate.customer) {
    println "${customer.@name} works for ${customer.@company}"
}
```
Result:
```
Bill Gates works for Microsoft
Tim Cook works for Apple
Larry Ellison works for Oracle
```

**A friendly language**

## Community driven but corporate backed

# What Groovy can do for you 

- Groovy for the busy Java professional
- Groovy for the polyglot programmer
- Groovy for pragmatic programmers, extremos, and agilists

# Running Groovy 

- https://groovy-playground.appspot.com/

## Using groovysh for a welcome message
```
> groovysh
Groovy Shell (3.0.7, JVM: 15.0.1)
Type ':help' or ':h' for help.
------------------------------------------------------------------------------------------------
groovy:000>
```

## Using groovyConsole 

## Using the groovy command

Calculating the golden ratio with Gold.groovy:

![](https://drek4537l1klr.cloudfront.net/koenig2/Figures/021fig01_alt.jpg)

```scala
found golden ratio with fibo(23) as
46368 / 28657 = 1.6180339882
__________|________________
```

# Compiling and running Groovy 
## Compiling Groovy with groovyc 

```
groovyc -d classes Gold.groovy
```

## Running a compiled Groovy script with Java 
```groovy
java –cp %GROOVY_HOME%/embeddable/groovy-all-2.4.0.jar;classes Gold
found golden ratio with fibo(23) as
46368 / 28657 = 1.6180339882
```

groovyc -d classes Gold.groovy




# Groovy IDE and editor support 
- IntelliJ IDEA
- NetBeans
- Eclipse
- Groovy support in other editors

# Summary