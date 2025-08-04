# What's this?

This is naruto's dotfiles.

The dotfiles aims to provide easily and smoothly operaions and recognizable syntax eye-candy in zsh of a terminal.

# Prepare

Install commands that are necessary to setup the dotfiles.

## macOS

```bash
brew install git emacs tmux zsh eza bat less moar lstr
brew install cmake ghq gh fzf git-delta rg fd gradle yazi lazygit
brew install starship zoxide mcfly rustup-init mdcat
brew install sd hexyl
$(brew --prefix)/opt/fzf/install
rustup-init
```

# Clone the repository and Setup dotfiles

```bash
mkdir -p ~/projects
mkdir -p ~/.config
git clone --recursive https://github.com/Naruto/dotfiles.git ~/projects/dotfiles
DOTFILES_PATH=~/projects/dotfiles
ln -sfn ${DOTFILES_PATH}/.emacs.d ~/
ln -sfn ${DOTFILES_PATH}/.zshrc ~/
ln -sfn ${DOTFILES_PATH}/.zsh ~/
ln -sfn ${DOTFILES_PATH}/.tmux ~/
ln -sfn ${DOTFILES_PATH}/.tmux/.tmux.conf ~/
ln -sfn ${DOTFILES_PATH}/.tmux.conf.local ~/
ln -sfn ${DOTFILES_PATH}/.starship ~/
ln -sfn ${DOTFILES_PATH}/.config/lazygit ~/.config/
ln -sfn ${DOTFILES_PATH}/.config/yazi ~/.config/
ln -sfn ${DOTFILES_PATH}/.config/bat ~/.config/
ln -sfn ${DOTFILES_PATH}/.config/zsh-abbr ~/.config/
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

