# Writing Command-Line Applications

## Your First Application

All command-line applications essentially perform the following steps:

- Accept user input
- Perform some validation
- User the input to perform some custom task
- Present the result to the user; that is a success or failure

```sh
$ mkdir -p chap1/manual-parse
$ cd ch01/manual-parse
$ go mod init PracticalGo/ch01/manual-parse
```

> [source of the application](./manual-parse/main.go)

Run:

```sh
go build -o application
```

Result:

```sh
$ ./application
Invalid number of arguments
Usage: ./application <integer> [-h|--help]
A greeter application which prints the name you entered <integer> number
of times.
```

```sh
$ ./application -help
Usage: ./application <integer> [-h|-help]
A greeter application which prints the name you entered <integer> number
of times.
```

```sh
$ ./application 5
Your name please? Press the Enter key when done.
Joe Cool
Nice to meet you Joe Cool
Nice to meet you Joe Cool
Nice to meet you Joe Cool
Nice to meet you Joe Cool
Nice to meet you Joe Cool
```
