**[Docker入门教学](https://www.youtube.com/playlist?list=PLliocbKHJNwubNT2oK-xlB1GXTXuLFb0I)**

# Docker入门

## 创建环境
``` sh
mkdir docker-node && cd docker-node

touch hello.js
echo "console.log('I love this game.')" > hello.js
```
## 获取镜像
``` sh
docker pull node:15.3.0
```
## 创建自己的镜像
建立 `Dockerfile` 文件：

``` sh
FROM node:15.3.0

RUN mkdir /src
COPY hello.js /src

CMD ["node", "/src/hello.js"]
```

创建自己的镜像：

``` sh
docker build -t keer2345/docker-node:v01 .
```
## 运行镜像
``` sh
> docker run keer2345/docker-node:v01
I love this game.
```

## 推送到Github

``` sh
git add .
git commit -m "docker-node-1"
git tag v01
git tag
git push --tags
```

## 关联到docker仓库

https://hub.docker.com/repository/create

可以关联 Github 创建 Docker 仓库。

关联完成后可以拉取到本地：

``` sh
docker pull keer2345/docker-node
```

# 指定执行的进程ENTRYPOINT
## 普通的执行
``` sh
docker run node:15.3.0 ls /etc
docker run node:15.3.0 cat /etc/hosts
docker run node:15.3.0 cat /etc/os-release

docker run node:15.3.0 node -v
```
## 实践
创建文件 Dockerfile

``` sh
FROM node:15.3.0

ENTRYPOINT ["node"]
CMD ["-v"]
```
运行：

``` sh
docker image build -t mynode .
docker run mynode
```
结果如下：

``` sh
v15.3.0
```

执行 JS 脚本：
``` sh
docker run mynode -e "console.log('hello')"
```


# 容器的使用：建立一个Nginx的Web服务器

``` sh
docker pull nginx:1.19.5-alpine
```

``` sh
docker run nginx:1.19.5-alpine ls
```
``` sh
> docker run nginx:1.19.5-alpine cat /etc/os-release
NAME="Alpine Linux"
ID=alpine
VERSION_ID=3.12.1
PRETTY_NAME="Alpine Linux v3.12"
HOME_URL="https://alpinelinux.org/"
BUG_REPORT_URL="https://bugs.alpinelinux.org/"
```

``` sh
docker run nginx:1.19.5-alpine ls -R -l /etc/nginx
docker run nginx:1.19.5-alpine cat /etc/nginx/nginx.conf
```
运行容器：
``` sh
docker run --name my-nginx -d -p 8080:80 nginx:1.19.5-alpine
```

``` sh
docker restart my-nginx
docker start my-nginx
docker stop my-nginx
docker stop $(docker ps -aq)

docker rm my-nginx

docker stop $(docker ps -aq)             # 停止所有容器
docker rm ${docker ps -aq}               # 删除所有容器
docker rm $(docker ps -qf status=exited) # 删除已停止的容器

docker rmi $(docker images -q)           # 删除所有镜像
```


新建文件 `/home/keer/myweb/index.html`：

``` html
<h1>Hello Docker world.</h1>
```

运行容器：

``` sh
docker run --name my-web -d -p 8080:80 -v /home/keer/myweb:/usr/share/nginx/html:ro nginx:1.19.5-alpine
```

访问 `curl http://127.0.0.1:8080`。
