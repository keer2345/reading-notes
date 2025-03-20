- [Docker入门教学](https://www.youtube.com/playlist?list=PLliocbKHJNwubNT2oK-xlB1GXTXuLFb0I)


----

> 解决docker: Error response from daemon: Get "https://registry-1.docker.io/v2/": net/http: request canceled while waiting for connection (Client.Timeout exceeded while awaiting headers).

首先进入`/etc/docker/daemon.json`文件

然后在里面加入下面的配置

```sh
{
"registry-mirrors": ["https://docker.registry.cyou",
"https://docker-cf.registry.cyou",
"https://dockercf.jsdelivr.fyi",
"https://docker.jsdelivr.fyi",
"https://dockertest.jsdelivr.fyi",
"https://mirror.aliyuncs.com",
"https://dockerproxy.com",
"https://mirror.baidubce.com",
"https://docker.m.daocloud.io",
"https://docker.nju.edu.cn",
"https://docker.mirrors.sjtug.sjtu.edu.cn",
"https://docker.mirrors.ustc.edu.cn",
"https://mirror.iscas.ac.cn",
"https://docker.rainbond.cc"]
}
```


然后在终端重新启动一下docker
```sh
systemctl daemon-reload
systemctl restart docker
```
然后再拉镜像
