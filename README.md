# What's this?

This is naruto's dotfiles.

These dotfiles provide a smooth experience and eye-catching syntax highlighting for Zsh.

# Prepare

Install commands that are necessary to setup the dotfiles.

## macOS

```bash
brew install git emacs tmux zsh eza bat less moor lstr
brew install cmake ghq gh fzf git-delta rg fd ast-grep gradle yazi lazygit
brew install starship zoxide rustup-init mdcat bottom
brew install sd hexyl
brew install d-kuro/tap/gwq
rustup-init
```

# Clone the repository and Setup dotfiles

```bash
git clone --recursive https://github.com/Naruto/dotfiles.git ~/projects/dotfiles
cd ~/projects/dotfiles
./install.sh
```

Prepend the below section to `~/.gitconfig` file

```ini
[include]
    path = ~/projects/dotfiles/.gitconfig
```

Execute the below commands in zsh shell.

```bash
ya pkg add yazi-rs/plugins:toggle-pane
```

