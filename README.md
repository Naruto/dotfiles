# What's this?

This is naruto's dotfiles.

    .emacs.d/  - emacs dotfiles
    .tmux.conf - tmux dotfile
    .zshrc     - zsh dotfile

# Prepare

install applications

## Ubuntu

    $ sudo apt-get -y install git
    $ sudo apt-get -y install emacs tmux zsh fonts-vlgothic 
    $ sudo apt-get -y install global id-utils gdb
    $ sudo apt-get -y install cmigemo migemo-el

# Get and Set up dotfiles

    $ git clone https://github.com/Naruto/dotfiles.git
    $ DOTFILES_PATH=$(pwd)/dotfiles
    $ ln -sf ${DOTFILES_PATH}/.emacs.d ~/
    $ ln -sf ${DOTFILES_PATH}/.tmux.conf ~/
    $ ln -sf ${DOTFILES_PATH}/.zshrc ~/

