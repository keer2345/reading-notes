# Getting Started

**本章讲述**

- 编写约定和可测试代码的重要性
- 介绍 Go 特性

## 为什么要阅读本书

“Go 入门容易，却难精通。”

本书两个目标

1. Go 的约定和可读
1. Go 的可测试和可维护

我们浏览命令行工具，设计并发程序，结构化 HTTP 服务端和客户端， 通过有效的用户接口编写适应性代码。

## 代码约定

“程序是给人们阅读的，计算机只是执行它们。”

Go 代码应该像这样：
- 简单
- 可读
- 显示的
- 实用
- 可测试

## 可测试的代码
## Go 为什么存在
## Go 擅长什么
- 客户端和服务端程序
- 分布式网络编程
- 命令行工具

## Hello World

```go
package main
import "fmt"

func main() {
    message := "Hello World!"
    fmt.Println(message)
}
```
## 编译和静态库
## Go 的运行
- Go scheduler
- Garbage collector
## 指针
## 并发
- Goroutines
- Go Scheduler
- Channels
- Wrap up
## 类型系统
- 嵌入取代了继承
- 隐式接口
## 标准库
## 工具
