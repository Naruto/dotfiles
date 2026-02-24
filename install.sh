#!/usr/bin/env bash

set -e

DOTFILES_PATH=~/projects/dotfiles

echo "Setting up dotfiles..."

mkdir -p ~/projects
mkdir -p ~/.config
mkdir -p ~/.cargo

echo "Creating symlinks..."
ln -sfn ${DOTFILES_PATH}/.emacs.d ~/
ln -sfn ${DOTFILES_PATH}/.zshrc ~/
ln -sfn ${DOTFILES_PATH}/.zsh ~/
ln -sfn ${DOTFILES_PATH}/.zfunc ~/
ln -sfn ${DOTFILES_PATH}/.tmux ~/
ln -sfn ${DOTFILES_PATH}/.tmux/.tmux.conf ~/
ln -sfn ${DOTFILES_PATH}/.tmux.conf.local ~/
ln -sfn ${DOTFILES_PATH}/.starship ~/
ln -sfn ${DOTFILES_PATH}/.config/lazygit ~/.config/
ln -sfn ${DOTFILES_PATH}/.config/yazi ~/.config/
ln -sfn ${DOTFILES_PATH}/.config/bat ~/.config/
ln -sfn ${DOTFILES_PATH}/.config/zsh-abbr ~/.config/
ln -sfn ${DOTFILES_PATH}/.config/gwq ~/.config/
ln -sfn ${DOTFILES_PATH}/.cargo/config ~/.cargo/

echo "Dotfiles setup complete!"
