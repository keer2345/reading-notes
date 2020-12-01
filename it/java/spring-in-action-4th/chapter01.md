**Chapter 01 Springing into Action**


本章涵盖：
- Spring 的 bean容器
- 浏览Spring核心模块
- Spring生态系统的强大
- Spring的亮点

# 简单的Java开发
创建Spring是为了解决企业应用程序开发的复杂性，并使使用普通Javabean实现以前只有EJB才能实现的功能成为可能。但是Spring的用处不仅限于服务器端开发。任何Java应用程序都可以从Spring中获益，比如简单性、可测试性和松耦合性。

Bean通过任意名称……虽然Spring在引用应用组件时使用 *bean* 和 *JavaBean* 来称呼，但并不意味着Spring必须遵守 JavaBean 规范，Spring 组件可以是任意类型的POJO。本书中，我们假设定义了一个松散的JavaBean，它与POJO是同一个概念.

通过本书可以了解到Spring做了很多事情,但Spring提供的几乎所有东西的根源是一些基本思想，都集中在Spring的基本任务上：简化Java开发。

Spring针对Java的复杂性采用了四种关键策略：
1. 使用POJO进行轻量级和微创开发
1. 通过依赖注入（DI）和面向接口（interface orientation）的松散耦合
1. 通过切面（aspect）和公共约定声明式编程

## 释放POJO的力量
Spring从不强迫你实现Spring特定接口或扩展特定类。相反，基于Spring的应用程序中的类通常没有被Spring使用的迹象。在最坏的情况下，一个类可以用Spring的一个注释来注释，但是在其他方面它是一个POJO。 

通过 `HelloWorldBean` 来展示 Spring 不会对其有任何不合理的要求：

``` java
package com.habuma.spring;

public class HelloWorldBean {
  public String sayHello() {
    return "Hello World";
  }
}
```

可以看到，这个简单的 Spring 组件没有什么特别之处。Spring非入侵式编程模式意味着这个类的功能在Spring应用和非Spring应用中一样好。

尽管简单形式的POJO，但却强大。接下来可以看到依赖注入（DI）如何帮助应用程序在对象之间解耦。

## 依赖注入
*Dependency Injection*

**它是如何如果做的？**

任何非凡的应用都是由两个或多个类相互协作以执行业务逻辑组成的。传统的形式是每个对象负责获得它对与其写作对象的引用（依赖项）：
``` java
package com.springinaction.knghts;

public class DamselRescuingKnight implements Knight {
  private RescueDamselQuest quest;
  
  public DamselRescuingKnight() {
    this.quest = new RescueDamselQuest();   // Tightly coupled to RescueDamselQuest
  }
  
  public void embarkOnQuest() {
    quest.embark();
  }
}
```

可以看到，`DamselRescuingKnight` 在构造函数中创建了自己的 `quest`，是一个 `RescueDamselQuest`。

耦合是把双刃剑。一方面，紧密耦合的代码难以测试、重用和理解，易于出现 bug（通常修复旧bug会导致新bug的出现）；另一方面，耦合又是必要的——完全不耦合的代码什么也做不了。

通过DI，对象在创建时通过第三方给出它们的依赖，对象不应创建或获取依赖：
``` java
package com.springinaction.knights;

public class BraveKnight implements Knight {
  private Quest quest;

  public BraveKnight(Quest quest) {     // Quest is injected
    this.quest = quest; 
  }
  
  public void embarkOnQuest() { 
    quest.embark();
  }
}
```

为了测试 `BraveKnight`，通过模拟 Quest 来注入：
``` java
package com.springinaction.knights;

import static org.mokito.Mockito.*;
import org.juint.Test;

public class BraveKnightTest {
  
  @Test
  public void knightShouldEmbarkOnQuest() {
    Quest mockQuest = mock(Quest.class);    // Create mock Quest
    BraveKnight knight = new BraveKnight(mockQuest);    // Inject mock Quest
    kinght.embarkOnQuest();
    
    verify(mockQuest, times(1)).embark();
  }
}
```

**将 Quest 注入到 Kngiht**

现在可以想 `BraveKnight` 提供任何的 quest，但是如何制定具体提供哪一个 `Quest` 呢？假设你想让 `BraveKnight` 完成杀龙的任务，那可能 `SlayDragonQuest` 更为适合：

``` java
package com.springinaction.knights;

import java.io.PrintStream;

public class SlayDragonQuest implements Quest {
  
  private PrintStream stream;
  
  public SlayDragonQuest(PrintStream stream) {
    this.stream = stream;
  }
  
  public void embark() {
    stream.println("Embarking on quest to slay the dragon!");
  }
}
```

`SlayDragonQuest` 实现了 `Quest` 接口，并通过构造函数调用了 `PrintStream`。问题来了，我们如何将 `SlayDragonQuest` 提供给 `BraveKnight`？如何将 `PrintStream` 提供给 `SlayDragonQuest`？

在 Spring 中有多种方式，通常是通过 XML，下面的 *knights.xml* 接通了 `BraveKnight`，`SlayDragonQuest` 和 `PrintStream`：
``` xml
<?xml version="1.0" encoding="UTF-8"?>
<beans xmlns="http://www.springframework.org/schema/beans"
xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
xsi:schemaLocation="http://www.springframework.org/schema/beans
                    http://www.springframework.org/schema/beans/spring-beans.xsd">

  <bean id="knight" class="com.springinaction.knights.BraveKnight">
    <!-- Inject quest bean -->
    <constructor-arg ref="quest" />
  </bean>
  
  <!-- Create SlayDragonQuest -->
  <bean id="quest" class="com.springinaction.knights.SlayDragonQuest">
    <constructor-arg value="#{T(System).out}" />
  </bean>

</beans>
```
这里，`BraveKnight` 和 `SlayDragonQuest` 在 Spring 中被声明为 bean。如果 XML 配置不适合你，也可以通过 Java 来配置：
``` java
package com.springinaction.knights.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.springinaction.knights.BraveKnight; 
import com.springinaction.knights.Knight;
import com.springinaction.knights.Quest;
import com.springinaction.knights.SlayDragonQuest;

@Configuration
public class KnightConfig {

  @Bean
  public Knight knight() {
    return new BraveKnight(quest());
  }

  @Bean
  public Quest quest() {
    return new SlayDragonQuest(System.out); 
  }
}
```

让它工作起来：
``` java
package com.springinaction.knights;

import org.springframework.context.support.ClassPathXmlApplicationContext;

public class KnightMain {

  public static void main(String[] args) throws Exception { // Load Spring context
    ClassPathXmlApplicationContext context = 
      new ClassPathXmlApplicationContext( "META-INF/spring/knight.xml");

    Knight knight = context.getBean(Knight.class);  // Get knight bean
    knight.embarkOnQuest();
    context.close();
  }
}
```

## 应用切面
尽管 DI  让组件变得松散，面向切面编程（AOP）可以捕获到在整个应用中可以重复使用的组件的功能。AOP 通常被认为是在软件系统中促进关注点分离的技术。系统又多个组件组成，每个组件有特定的任务，但通常组件除了核心功能外，还担负着额外的任务。类似日志、实物管理、安全的系统服务通常除了核心功能外还有其他的功能，在系统中这些组件被成为跨系统的组件，因为他们通常被认为跨系统的关注点。

``` java
package com.springinaction.knights;

import java.io.PrintStream;

public class Minstrel {

  private PrintStream stream;

  public Minstrel(PrintStream stream) { 
    this.stream = stream;
  }

  public void singBeforeQuest() {
    stream.println("Fa la la, the knight is so brave!");
  }

  public void singAfterQuest() {
    stream.println("Tee hee hee, the brave knight " +
      "did embark on a quest!");
  } 
}
```
