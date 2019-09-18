# What's this?

This is naruto's dotfiles.

```
.
|-- .emacs.d     - emacs dotfiles
|-- .peco
|-- .percol.d
|-- .spacemacs.d - spacemacs dotfiles
|-- .tmux.conf   - tmux dotfile
|-- .zsh         - add zsh auto suggestions files  
|-- .zshrc       - zsh dotfile
`-- README.md    - this file
```

# Prepare

install applications

## macOS

```sh
brew install git emacs tmux zsh exa
```

```sh
brew install cmake ghq hub lv mono peco rg ag tree gradle
```


## Ubuntu

must install

```sh
sudo apt-get -y install git
sudo apt-get -y install emacs tmux zsh fonts-vlgothic 
```

shall install

```sh
sudo add-apt-repository ppa:webupd8team/java
sudo apt-get update
sudo apt-get -y install global id-utils gdb  # for c developer
sudo apt-get -y install cmigemo migemo-el    # incremental japanese search
sudo apt-get -y install xclip                # seemless copy&paste between emacs and another X applications.
sudo apt-get -y install lv                   # high convenience pager
sudo apt-get install silversearcher-ag
sudo apt-get install ack-grep
curl -fsSkL https://raw.github.com/cask/cask/master/go | python
pip install percol
pip install pylint
gem install rubocop ruby-lint
sudo apt-get install clang
pip install rope
pip install jedi
pip install flake8
sudo apt-get -y install oracle-java8-installer
sudo apt-get -y install curl
```

## common

For chromium developer

```sh
mkdir -p ~/projects/
cd ~/projects
git clone https://chromium.googlesource.com/chromium/tools/depot_tools.git
```

For Android Application developer

```sh
sudo apt-get install libc6-i386 lib32stdc++6 lib32z1 
sudo apt-get install ant gradle
# download android studio from http://developer.android.com/sdk/index.html
unzip android-studio-ide-XXX.XXXXXXXX-linux.zip
mkdir -p /opt/android/
mv android-studio /opt/android
```

For mobile platform(ex: Android, Tizen) developer.

```sh
sudo apt-get -y install curl
mkdir -p ~/bin
curl http://commondatastorage.googleapis.com/git-repo-downloads/repo > ~/bin/repo
chmod a+x ~/bin/repo
```

# Get and Set up dotfiles

```sh
mkdir -p ~/projects
git clone https://github.com/Naruto/dotfiles.git ~/projects/dotfiles
DOTFILES_PATH=~/projects/dotfiles
pushd ${DOTFILES_PATH}
git submodule update --init --recursive
ln -sfn ${DOTFILES_PATH}/.emacs.d ~/
ln -sfn ${DOTFILES_PATH}/.spacemacs.d ~/
ln -sfn ${DOTFILES_PATH}/.tmux.conf ~/
ln -sfn ${DOTFILES_PATH}/.zshrc ~/
ln -sfn ${DOTFILES_PATH}/.zsh ~/
ln -sfn ${DOTFILES_PATH}/.percol.d ~/
ln -sfn ${DOTFILES_PATH}/.peco ~/
popd
```
