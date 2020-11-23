https://www.bilibili.com/video/BV165411h77e?p=1


# 购买阿里云ESC
选择系统 CentOS 8.2
# 安装软件
## JDK
- 安装
```
mkdir /usr/java
tar -zxvf jdk-15.0.1_linux-x64_bin.tar.gz -C /usr/java
```
- JDK环境变量
添加下面内容在文件 `/etc/profile` 的最后:
```
# set java enviroment
export JAVA_HOME=/usr/java/jdk-15.0.1
export CLASSPATH=.:${JAVA_HOME}/lib
export PATH=${JAVA_HOME}/bin:$PATH
```
- 校验
```
source /etc/profile
java -version
```
## Tomcat
- 安装
```
tar -zxvf apache-tomcat-8.5.60.tar.gz -C /usr/local/
```
- 运行

```
cd /usr/local/apache-tomcat-8.5.60/bin/
./startup.sh
```
- 开放 `8080` 端口
在阿里云”配置规则“处，配置”安全组规则“，添加**入方向**的 `8080` 端口规则，即可访问 tomcat 页面。

## MariaDB
参考 MariaDB 官方的安装方法：

- 添加文件 `/etc/yum.repos.d/CentOS-MariaDB.repo`：
```
# MariaDB 10.5 CentOS repository list - created 2020-11-22 12:21 UTC
# https://mariadb.org/download/
[mariadb]
name = MariaDB
baseurl = https://mirrors.tuna.tsinghua.edu.cn/mariadb/yum/10.5/centos8-amd64
module_hotfixes=1
gpgkey=https://mirrors.tuna.tsinghua.edu.cn/mariadb/yum/RPM-GPG-KEY-MariaDB
gpgcheck=1
```
- 安装
```
sudo dnf install MariaDB-server
sudo systemctl start mariadb
```
- 登录到数据库
用 `mysql -uroot` 命令登录到 MariaDB，此时 `root` 账户的密码为空。
- 进行MariaDB的相关简单配置
运行 `mysql_secure_installation`:

``` sh
#输入root(mysql)的密码。默认没有，直接回车
Enter current password for root (enter for none): 
#是否切换到unix套接字身份验证[Y/n]
Switch to unix_socket authentication [Y/n] n
#是否设置root密码
Change the root password? [Y/n]
#如果选Y，就输入2次密码
New password:
Re-enter new password:
#是否删除匿名用户?(就是空用户)，建议删除
Remove anonymous users? [Y/n]
#是否不允许远程root登录
Disallow root login remotely? [Y/n]
#是否删除test数据库
Remove test database and access to it? [Y/n]
#是否加载权限使之生效
Reload privilege tables now? [Y/n]
```


- 设置开机启动
```
sudo systemctl enable mariadb --now
```

- 使用 MariaDB
登录：

``` sh
sudo mysql -u root -p
```
- 需要外部工具访问的话，需要授权：
``` sh
grant all privileges on *.* to 'root'@'%' identified by '******'; flush privileges;
```

``` sh
## 创建数据库
MariaDB [(none)]> CREATE DATABASE mydb;
 
## 创建账户
MariaDB [(none)]> CREATE USER 'dbuser'@'localhost' IDENTIFIED BY 'secret';
 
## 赋予权限
MariaDB [(none)]> GRANT ALL ON mydb.* TO 'dbuser'@'localhost';
 
## 重载权限 
MariaDB [(none)]> FLUSH PRIVILEGES;
```

- 使用 phpMyAdmin
https://tecadmin.net/install-phpmyadmin-on-centos/

## Jemalloc

``` sh
sudo dnf install jemalloc
sudo dnf install jemalloc-devel
```
## Galera
``` sh
sudo dnf install galera
```

## FastDFS

- 下载

``` sh
wget https://github.com/happyfish100/libfastcommon/archive/V1.0.43.tar.gz -SO libfastcommon.tar.gz
wget https://github.com/happyfish100/fastdfs/archive/V6.06.tar.gz -SO fastdfs.tar.gz
wget https://github.com/happyfish100/fastdfs-nginx-module/archive/V1.22.tar.gz -SO fastdfs-nginx-module.tar.gz
```
- 依赖包
``` sh
dnf install gcc-c++
dnf install pcre-devel
dnf install zlib zlib-devel
dnf install openssl openssl-devel
```

- 安装 libfastcommon

``` sh
tar -zxf libfastcommon.tar.gz
cd libfastcommon

./make.sh
./make.sh install

ll /usr/lib64/libfast*
ll /usr/lib/libfastcommon.so
```
- 安装 FastDFS

``` sh
tar -zxf fastdfs.tar.gz
cd fastdfs

./make.sh
./make.sh install

ll /usr/bin/fdfs*
ll /etc/fdfs/

cp * /etc/fdfs/
ls -ltr /etc/fdfs/
```
- 编辑文件 `/etc/fdfs/tracker.conf`:

``` sh
# the base path to store data and log files
# base_path = /home/yuqing/fastdfs
base_path = /fastdfs/tracker
```
- 创建以下四个目录

``` sh
mkdir -p /fastdfs/{tracker,storage,client,tmp}
```

- 启动

``` sh
cd /usr/bin/
fdfs_trackerd /etc/fdfs/tracker.conf
fdfs_trackerd /etc/fdfs/tracker.conf restart
```
- 编辑 `/etc/fdfs/storage.conf`：

``` sh
# group_name = group1
group_name = wdzl

# base_path = /home/yuqing/fastdfs
base_path = /fastdfs/storage

#store_path0 = /home/yuqing/fastdfs
store_path0 = /fastdfs/storage
#store_path1 = /home/yuqing/fastdfs2

#tracker_server = 192.168.209.121:22122
#tracker_server = 192.168.209.122:22122
# 换成云服务器上的私网IP
tracker_server = 172.*.*.*:22122
```
- 启动

``` sh
cd /usr/bin
./fdfs_storaged  /etc/fdfs/storage.conf
./fdfs_storaged  /etc/fdfs/storage.conf restart
```
- 编辑 `/etc/fdfs/client.conf`:

``` sh
# 换成云服务器上的私网IP
# tracker_server = 192.168.0.196:22122
# tracker_server = 192.168.0.197:22122
tracker_server = 172.*.*.*:22122
```


- 测试图像上传

``` sh
cd ~
fdfs_test /etc/fdfs/client.conf upload /root/tools/test1.jpg
```
执行结果：
```
This is FastDFS client test program v6.06

Copyright (C) 2008, Happy Fish / YuQing

FastDFS may be copied only under the terms of the GNU General
Public License V3, which may be found in the FastDFS source kit.
Please visit the FastDFS Home Page http://www.fastken.com/
for more detail.

[2020-11-23 09:58:33] DEBUG - base_path=/fastdfs/client, connect_timeout=5, network_timeout=60, tracker_server_count=1, anti_steal_token=0, anti_steal_secret_key length=0, use_connection_pool=0, g_connection_pool_max_idle_time=3600s, use_storage_id=0, storage server id count: 0

tracker_query_storage_store_list_without_group:
	server 1. group_name=, ip_addr=172.*.*.*, port=23000

group_name=wdzl, ip_addr=172.*.*.*, port=23000
storage_upload_by_filename
group_name=wdzl, remote_filename=M00/00/00/rBJb8V-7F0mAFjxlAAX4vWTO1mA466.jpg
source ip address: 172.*.*.*
file timestamp=2020-11-23 09:58:33
file size=391357
file crc32=1691276896
example file url: http://172.*.*.*/wdzl/M00/00/00/rBJb8V-7F0mAFjxlAAX4vWTO1mA466.jpg
storage_upload_slave_by_filename
group_name=wdzl, remote_filename=M00/00/00/rBJb8V-7F0mAFjxlAAX4vWTO1mA466_big.jpg
source ip address: 172.*.*.*
file timestamp=2020-11-23 09:58:34
file size=391357
file crc32=1691276896
example file url: http://172.*.*.*/wdzl/M00/00/00/rBJb8V-7F0mAFjxlAAX4vWTO1mA466_big.jpg
```
- 配置 `fastdfs-nginx-module/src/config`，把文件中的 `local/` 去掉：
``` sh
ngx_addon_name=ngx_http_fastdfs_module

if test -n "${ngx_module_link}"; then
    ngx_module_type=HTTP
    ngx_module_name=$ngx_addon_name
    ngx_module_incs="/usr/include"
    ngx_module_libs="-lfastcommon -lfdfsclient"
    ngx_module_srcs="$ngx_addon_dir/ngx_http_fastdfs_module.c"
    ngx_module_deps=
    CFLAGS="$CFLAGS -D_FILE_OFFSET_BITS=64 -DFDFS_OUTPUT_CHUNK_SIZE='256*1024' -DFDFS_MOD_CONF_FILENAME='\"/etc/fdfs/mod_fastdfs.conf\"'"
    . auto/module
else
    HTTP_MODULES="$HTTP_MODULES ngx_http_fastdfs_module"
    NGX_ADDON_SRCS="$NGX_ADDON_SRCS $ngx_addon_dir/ngx_http_fastdfs_module.c"
    CORE_INCS="$CORE_INCS /usr/include"
    CORE_LIBS="$CORE_LIBS -lfastcommon -lfdfsclient"
    CFLAGS="$CFLAGS -D_FILE_OFFSET_BITS=64 -DFDFS_OUTPUT_CHUNK_SIZE='256*1024' -DFDFS_MOD_CONF_FILENAME='\"/etc/fdfs/mod_fastdfs.conf\"'"
fi
```

## Nginx
- 安装
``` sh
tar -zxvf nginx-1.19.4.tar.gz
```

注意修改最后一行的路径：
``` sh
 ./configure \
 --prefix=/usr/local/nginx \
 --pid-path=/var/run/nginx/nginx.pid \
 --lock-path=/var/lock/nginx.lock \
 --error-log-path=/var/log/nginx/error.log \
 --http-log-path=/var/log/nginx/access.log \
 --with-http_gzip_static_module \
 --http-client-body-temp-path=/var/temp/nginx/client \
 --http-proxy-temp-path=/var/temp/nginx/proxy \
 --http-fastcgi-temp-path=/var/temp/nginx/fastcgi \
 --http-uwsgi-temp-path=/var/temp/nginx/uwsgi \
 --http-scgi-temp-path=/var/temp/nginx/scgi \
 --add-module=/root/tools/fastdfs-nginx-module-1.22/src
```

``` sh
make
make install
ls /usr/local/nginx
```
- 配置

``` sh
cp /root/tools/fastdfs-nginx-module-1.22/src/mod_fastdfs.conf /etc/fdfs/
```
编辑 `/etc/fdfs/mod_fastdfs.conf`：

``` sh
# the base path to store log files
# base_path=/tmp
base_path=/fastdfs/tmp

# 改成私网IP
# FastDFS tracker_server can ocur more than once, and tracker_server format is
#  "host:port", host can be hostname or ip address
# valid only when load_fdfs_parameters_from_tracker is true
# tracker_server=tracker:22122
tracker_server=172.*.*.*:22122

# the group name of the local storage server
# group_name=group1
group_name=wdzl

# 满足 web 端、手机端访问
# if the url / uri including the group name
# set to false when uri like /M00/00/00/xxx
# set to true when uri like ${group_name}/M00/00/00/xxx, such as group1/M00/xxx
# default value is false
url_have_group_name = true

# store_path#, based 0, if store_path0 not exists, it's value is base_path
# the paths must be exist
# must same as storage.conf
# store_path0=/home/yuqing/fastdfs
store_path0=/fastdfs/storage
#store_path1=/home/yuqing/fastdfs1
```
- 编辑 `/usr/local/nginx/conf/nginx.conf`，在 `listen 80` 的上方添加一个 `server`：
``` sh
    server {
        listen 88;
        server_name     47.113.95.17;
        location /wdzl/M00{
            ngx_fastdfs_module;
        }
    }

    server {
        listen       80;
        
        # ...
    }
```

- 测试

``` sh
mkdir -p /var/temp/nginx/client
cd /usr/local/nginx/sbin
./nginx -t
```
测试结果：

``` sh
ngx_http_fastdfs_set pid=39334
nginx: the configuration file /usr/local/nginx/conf/nginx.conf syntax is ok
nginx: configuration file /usr/local/nginx/conf/nginx.conf test is successful
```


- 在阿里云服务器安全组规则添加“入访问”端口 `88`
- 访问下面的网址就可以浏览到前面上传的图片了：
http://47.113.95.17:88/wdzl/M00/00/00/rBJb8V-7F0mAFjxlAAX4vWTO1mA466.jpg

# 设置反向代理
`/usr/local/nginx/conf/nginx.conf`:

``` sh
server {
    listen 88;
    server_name     47.113.95.17;
    location /wdzl/M00{
        ngx_fastdfs_module;
    }
}

upstream tomcats{
    server 172.18.91.241:8080;
}

server {
    listen       80;
    server_name  localhost;

    #charset koi8-r;

    #access_log  logs/host.access.log  main;

    location / {
        # root   html;
        proxy_pass http://tomcats;
        index  index.html index.htm;
    }
    
    # ...
}
```

``` sh
cd /usr/local/nginx/sbin
./nginx -s reload;
```

这时候我们就可以在安全组规则将**入方向**的 `8080` 端口删除掉了，然后通过 `http://47.113.95.17/` 访问 Tomcat。

# 后端
...

# 前段
...
