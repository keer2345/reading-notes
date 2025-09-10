# zsh
## 安装
1. 安装 zsh
2. [oh-my-zsh](https://ohmyz.sh):
   
  ```sh
  sh -c "$(wget https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh -O -)"
  ```
## 配置
参考 [~/.zshrc](https://github.com/keer2345/dotfiles_arch/blob/main/.zshrc)

## 插件
### 示例

```sh
# 找到 plugins 行，添加你需要的插件
plugins=(
    git           # Git 相关功能
    autojump      # 目录快速跳转
    zsh-autosuggestions    # 命令建议
    zsh-syntax-highlighting # 语法高亮
    docker        # Docker 命令补全
    kubectl       # Kubernetes 命令补全
    npm           # NPM 命令补全
    python        # Python 相关功能
    history       # 历史命令增强
    sudo          # 按两次 ESC 添加 sudo
)
```

对于非官方插件（如 zsh-autosuggestions），需要先安装：
```sh
# 安装 zsh-autosuggestions
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions

# 安装 zsh-syntax-highlighting
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
```
### 常用插件列表
#### 核心插件
```sh
plugins=(
    git
    autojump
    zsh-autosuggestions
    zsh-syntax-highlighting
    history
    sudo
)
```

#### 开发相关插件：
```zsh
plugins=(
    node
    npm
    python
    ruby
    golang
    rust
    docker
    docker-compose
    kubectl
    terraform
)
```
####工具增强插件：
```zsh
plugins=(
    tmux
    vagrant
    aws
    gcloud
    heroku
    ssh-agent
    encode64
    extract
)
```
### 推荐组合
#### 基础高效组合：
```zsh
plugins=(
    git
    autojump
    zsh-autosuggestions
    zsh-syntax-highlighting
    history
    sudo
    extract
    web-search
)
```
#### 开发者完整组合：

```zsh
plugins=(
    git
    autojump
    zsh-autosuggestions
    zsh-syntax-highlighting
    docker
    docker-compose
    kubectl
    npm
    node
    python
    pip
    poetry
    rust
    golang
    terraform
    aws
    gcloud
    fzf
)
```
配置完成后，记得运行：

```sh
source ~/.zshrc
```

或者重新打开终端使配置生效。这样你就可以享受 Zsh 插件带来的强大功能了！
