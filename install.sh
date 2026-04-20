#!/usr/bin/env bash

set -e

DOTFILES_PATH=$(cd "$(dirname "$0")" && pwd)

echo "Setting up dotfiles..."

mkdir -p ~/projects
mkdir -p ~/.config
mkdir -p ~/.cargo

link() {
  local src="$1"
  local dest="$2"
  if [[ -L "$dest" && "$(readlink "$dest")" == "$src" ]]; then
    echo "  skip (already linked): $dest"
  else
    ln -sfn "$src" "$dest"
    echo "  linked: $dest -> $src"
  fi
}

echo "Creating symlinks..."
link "${DOTFILES_PATH}/.emacs.d" ~/".emacs.d"
link "${DOTFILES_PATH}/.zshrc" ~/".zshrc"
link "${DOTFILES_PATH}/.zsh" ~/".zsh"
link "${DOTFILES_PATH}/.zfunc" ~/".zfunc"
link "${DOTFILES_PATH}/.tmux" ~/".tmux"
link "${DOTFILES_PATH}/.tmux/.tmux.conf" ~/".tmux.conf"
link "${DOTFILES_PATH}/.tmux.conf.local" ~/".tmux.conf.local"
link "${DOTFILES_PATH}/.starship" ~/".starship"
link "${DOTFILES_PATH}/.config/lazygit" ~/.config/"lazygit"
link "${DOTFILES_PATH}/.config/yazi" ~/.config/"yazi"
link "${DOTFILES_PATH}/.config/bat" ~/.config/"bat"
link "${DOTFILES_PATH}/.config/zsh-abbr" ~/.config/"zsh-abbr"
link "${DOTFILES_PATH}/.config/gwq" ~/.config/"gwq"
link "${DOTFILES_PATH}/.config/ghostty" ~/.config/"ghostty"
link "${DOTFILES_PATH}/.cargo/config" ~/.cargo/"config"
link "${DOTFILES_PATH}/.ripgreprc" ~/".ripgreprc"

echo "Dotfiles setup complete!"
