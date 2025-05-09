# Writing Command-Line Applications

## 1. <a name='YourFirstApplication'></a>Your First Application

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

> [source code](./manual-parse/main.go)

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

## Writing Unit Test

> [source code](./manual-parse/validate_args_test.go)

Run test:

```sh
go test -v
```

Result:

```sh
=== RUN   TestParseArgs
--- PASS: TestParseArgs (0.00s)
PASS
ok  	PracticalGo/ch01/manual-parse	0.002s
```

> [source code](./manual-parse/validate_args_test.go)

```sh
> go test -v

=== RUN   TestParseArgs
--- PASS: TestParseArgs (0.00s)
=== RUN   TestValidateArgs
--- PASS: TestValidateArgs (0.00s)
PASS
ok  	PracticalGo/ch01/manual-parse	0.002s
```

> [source code](./manual-parse/run_cmd_test.go)

```sh
> go test -v
=== RUN   TestParseArgs
--- PASS: TestParseArgs (0.00s)
=== RUN   TestRunCmd
--- PASS: TestRunCmd (0.00s)
=== RUN   TestValidateArgs
--- PASS: TestValidateArgs (0.00s)
PASS
ok  	PracticalGo/ch01/manual-parse	0.002s
```

Run the following command first to create a coverage profile:

```sh
> go test -coverprofile cover.out
PASS
coverage: 71.2% of statements
ok  	PracticalGo/ch01/manual-parse	0.002s
```

To see which parts of the code are covered, run the following:

```sh
> go tool cover -html=cover.out
```
