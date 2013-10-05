# What's this?

This is naruto's dotfiles.

    .emacs.d/  - emacs dotfiles
    .tmux.conf - tmux dotfile
    .zshrc     - zsh dotfile

# Get and Set up dotfiles

    $ git clone https://github.com/Naruto/dotfiles.git
    $ DOTFILES_PATH=$(pwd)/dotfiles
    $ ln -sf ${DOTFILES_PATH}/.emacs.d ~/
    $ ln -sf ${DOTFILES_PATH}/.tmux.conf ~/
    $ ln -sf ${DOTFILES_PATH}/.zshrc ~/

# Install applications

Ubuntu

    $ sudo apt-get install emacs tmux zsh
    $ sudo apt-get install cmigemo migemo-el

