# What's this?

This is naruto's dotfiles.

    .emacs.d/     - emacs dotfiles
    .spacemacs.d/ - spacemacs dotfiles
    .tmux.conf    - tmux dotfile
    .zshrc        - zsh dotfile

# Prepare

install applications

## Ubuntu

must install

    $ sudo apt-get -y install git
    $ sudo apt-get -y install emacs tmux zsh fonts-vlgothic 

shall install

    $ sudo add-apt-repository ppa:webupd8team/java
    $ sudo apt-get update
    $ sudo apt-get -y install global id-utils gdb  # for c developer
    $ sudo apt-get -y install cmigemo migemo-el    # incremental japanese search
    $ sudo apt-get -y install xclip                # seemless copy&paste between emacs and another X applications.
    $ sudo apt-get -y install lv                   # high convenience pager
    $ sudo apt-get install silversearcher-ag
    $ sudo apt-get install ack-grep
    $ curl -fsSkL https://raw.github.com/cask/cask/master/go | python
    $ pip install percol
    $ pip install pylint
    $ gem install rubocop ruby-lint
    $ sudo apt-get install clang
    $ pip install rope
    $ pip install jedi
    $ pip install flake8
    $ sudo apt-get install oracle-java8-installer

For chromium developer

    $ mkdir -p ~/projects/
    $ cd ~/projects
    $ git clone https://chromium.googlesource.com/chromium/tools/depot_tools.git

For mobile platform(ex: Android, Tizen) developer.

    $ sudo apt-get -y install curl
    $ mkdir -p ~/bin
    $ curl http://commondatastorage.googleapis.com/git-repo-downloads/repo > ~/bin/repo
    $ chmod a+x ~/bin/repo

For Android Application developer

    $ sudo apt-get install libc6-i386 lib32stdc++6 lib32z1 
    $ sudo apt-get install ant
    download android studio from http://developer.android.com/sdk/index.html
    $ unzip android-studio-ide-XXX.XXXXXXXX-linux.zip
    $ mkdir -p /opt/android/
    $ mv android-studio /opt/android

# Get and Set up dotfiles

    $ mkdir -p ~/projects
    $ git clone https://github.com/Naruto/dotfiles.git ~/projects/dotfiles
    $ DOTFILES_PATH=~/projects/dotfiles
    $ pushd ${DOTFILES_PATH}
    $ git submodule init
    $ git submodule update --recursive
    $ ln -sfn ${DOTFILES_PATH}/.emacs.d ~/
    $ ln -sfn ${DOTFILES_PATH}/.spacemacs.d ~/
    $ ln -sfn ${DOTFILES_PATH}/.tmux.conf ~/
    $ ln -sfn ${DOTFILES_PATH}/.zshrc ~/
    $ ln -sfn ${DOTFILES_PATH}/.zsh ~/
    $ ln -sfn ${DOTFILES_PATH}/.percol.d ~/
    $ ln -sfn ${DOTFILES_PATH}/.peco ~/
    $ popd

