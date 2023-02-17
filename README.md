# What's this?

This is naruto's dotfiles.

# Prepare

Install commands.

## macOS

```bash
brew install git emacs tmux zsh exa bat less mono coreutils
brew install cmake ghq gh fzf git-delta rg fd gradle nnn lazygit
brew install starship zoxide rustup-init mdcat htop
brew install reattach-to-user-namespace
brew install mas
$(brew --prefix)/opt/fzf/install # fzf setup
rustup-init # rust setup
```

# Get and Set up dotfiles

```bash
mkdir -p ~/projects
git clone https://github.com/Naruto/dotfiles.git ~/projects/dotfiles
DOTFILES_PATH=~/projects/dotfiles
pushd ${DOTFILES_PATH}
ln -sfn ${DOTFILES_PATH}/.emacs.d ~/
ln -sfn ${DOTFILES_PATH}/.zshrc ~/
ln -sfn ${DOTFILES_PATH}/.zsh ~/
ln -sfn ${DOTFILES_PATH}/.tmux ~/
ln -sfn ${DOTFILES_PATH}/.tmux.conf.local ~/
ln -sfn ${DOTFILES_PATH}/.starship ~/
popd
```

Add the below section to `~/.gitconfig` file

```init
[include]
    path = ~/projects/dotfiles/.gitconfig
```
