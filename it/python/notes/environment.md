# Environment
## pip3
Install

```py
sudo apt install python3-pip
```

`~/.pip/pip.conf`:
```sh
[global]
index-url = https://pypi.tuna.tsinghua.edu.cn/simple
```

Virtualenv:
```sh
pip3 install virtualenv
```

Use Virtualenv:
```sh
virtualenv .venv
source .venv/bin/activate
deactivate
```

## [Jupyter](https://jupyter.org)
```sh
pip3 install jupyterlab
jupyter lab
```
