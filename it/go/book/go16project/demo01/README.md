# Go 语言实战案例 -01-《命令行客户端》

- https://www.bilibili.com/video/BV1Zz421y7Ln

## Library

- [cobra](https://github.com/spf13/cobra)
- [viper](https://github.com/spf13/viper)

## Sample

**cmd/root.go**:

run:

`go run main.go --viper=true --config myconf.yaml -a nick1 -l apache1 -s local`

```sh
Using config file :  myconf.yaml
root cmd run begin
true nick1 myconf.yaml apache1 local
---------------------------------
nick1 apache1
root cmd run end
```

---

**cmd/init.go**:

run:

`go run main.go -s local add --viper=true -a nick1 -l apache1 --config myconf.yaml`

````sh
Using config file :  myconf.yaml
run init cmd begin
true nick1 myconf.yaml nick1 apache1 local
run init cmd end
```
````

**cmd/args.go**:

```sh
go run main.go cusargs a b
go run main.go args abc 123 nick
```
