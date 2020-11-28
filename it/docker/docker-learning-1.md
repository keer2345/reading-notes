**Docker Learning**

[Docker入门基础教程合集](https://www.ixigua.com/6825944588193104396/)

# Install
## Install on CentOS 8
**Docker**
参考 http://get.daocloud.io/#install-docker ，或者：
``` sh
dnf config-manager --add-repo=https://download.docker.com/linux/centos/docker-ce.repo

dnf list docker-ce
dnf install docker-ce -y

systemctl start docker
systemctl enable docker

docker info
```

``` sh
docker --version    # Docker version 19.03.13, build 4484c46d9d
```

**Docker Compose**

``` sh
dnf install curl -y

curl -L "https://github.com/docker/compose/releases/download/1.27.4/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
```
如果太慢，则选择下面的地址安装：
``` sh
curl -L https://get.daocloud.io/docker/compose/releases/download/1.27.4/docker-compose-`uname -s`-`uname -m` > /usr/local/bin/docker-compose 
```
``` sh
chmod +x /usr/local/bin/docker-compose

docker-compose --version    # docker-compose version 1.27.4, build 40524192
```

扩展阅读：[How to Run Containers with Podman on CentOS 8 / RHEL 8](https://www.linuxtechi.com/run-containers-podman-centos-8-rhel-8/)

# 基本命令

``` sh
docker help

docker images

docker search redis
docker pull redis
docker history redis
docker inspect redis

docker rmi redis[:latest] | ID
docker rmi $(docker images | grep "none" | awk '{print $3}')
```

# 容器的创建、启动和删除
查看帮助

``` sh
docker run --help
```

容器的创建
``` sh
docker run -it --name mytomcat tomcat:8.5.56-jdk8-openjdk /bin/bash
```
``` sh
docker run -di --name mytomcat tomcat:8.5.56-jdk8-openjdk /bin/bash
docker exec -it mytomcat /bin/bash
```


容器的查看与启停:

``` sh
docker ps [-a]
docker start/stop/restart [NAME | ID]

docker rm [NAME | ID]
docker rm $(docker ps -a -q -f status=exited)
```

# 目录挂载

## `--mount` 模式

``` sh
docker volume ls
docker volume create mytomcatvol
docker volume inspect mytomcatvol
docker volume rm mytomcatvol
```
- 桥接模式：
``` sh
docker run -di --name mytomcat2 --mount src=mytomcatvol,dst=/usr/local/tomcat -p 8083:8080 tomcat:8.5.60-jdk8-openjdk
```
- host模式：
``` sh
docker run -di --name mytomcat2 --mount src=mytomcatvol,dst=/usr/local/tomcat --net=host tomcat:8.5.60-jdk8-openjdk
```

## `-v` 模式

``` sh
docker run -di --name mytomcat4 -v /opt/tomcat:/usr/local/tomcat/webapps -p 8080:8080 tomcat:8.5.60-jdk8-openjdk
```
之后在宿主机的 `/opt/tomcat` 创建目录 `ROOT` 和文件 `index.html`，用以下命令进入容器也可以看到对于的文件：

``` sh
docker exec -it mytomcat4 /bin/bash

ls webapps
```
现在就可以访问页面了。

# 文件复制
- 复制到容器
``` sh
docker cp aaa.txt mytomcat:/opt
```
想知道 Tomcat 在容器里的工作目录，可以用 `docker inspect mytomcat4` 查看 *WorkingDir*。

- 复制到宿主机

``` sh
docker cp mytomcat:/usr/local/tomcat/conf/server.xml server.xml
```

# 图形化页面管理

``` sh
docker pull portainer/portainer
docker volume create portainer_data
docker run -d -p 9000:9000 -v /var/run/docker.sock:/var/run/docker.sock -v portainer_data:data portainer/portainer
```

# Mysql
# Redis
# Nginx
# Elasticsearch
# Dockerfile构建镜像
# RabbitMQ
# Docker Registry私服
