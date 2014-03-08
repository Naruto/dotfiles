# What's this?

This is naruto's dotfiles.

    .emacs.d/  - emacs dotfiles
    .tmux.conf - tmux dotfile
    .zshrc     - zsh dotfile

# Prepare

install applications

## Ubuntu

must install

    $ sudo apt-get -y install git
    $ sudo apt-get -y install emacs tmux zsh fonts-vlgothic 
    $ curl -fsSkL https://raw.github.com/cask/cask/master/go | python

shall install

    $ sudo apt-get -y install global id-utils gdb  # for c developer
    $ sudo apt-get -y install cmigemo migemo-el    # incremental japanese search
    $ sudo apt-get -y install xclip                # seemless copy&paste between emacs and another X applications.
    $ sudo apt-get -y install lv                   # high convenience pager
    $ sudo apt-get install silversearcher-ag
    $ sudo apt-get install ack-grep

For mobile platform(ex: Android, Tizen) developer.

    $ sudo apt-get -y install curl
    $ mkdir -p ~/bin
    $ curl http://commondatastorage.googleapis.com/git-repo-downloads/repo > ~/bin/repo
    $ chmod a+x ~/bin/repo

# Get and Set up dotfiles

    $ git clone https://github.com/Naruto/dotfiles.git
    $ DOTFILES_PATH=$(pwd)/dotfiles
    $ ln -sf ${DOTFILES_PATH}/.emacs.d ~/
    $ ln -sf ${DOTFILES_PATH}/.tmux.conf ~/
    $ ln -sf ${DOTFILES_PATH}/.zshrc ~/
    $ cd ${DOTFILES_PATH}
    $ git submodule init
    $ git submodule update --recursive
    
