# What's this?

This is naruto's dotfiles.

# Prepare

Install commands.

## macOS

```bash
brew install git emacs tmux zsh eza bat less
brew install cmake ghq gh fzf git-delta rg fd gradle yazi lazygit
brew install starship zoxide mcfly rustup-init mdcat htop
brew install sd hexyl
brew install reattach-to-user-namespace
$(brew --prefix)/opt/fzf/install # fzf setup
rustup-init # rust setup
```

## Windows

```
winget install gsudo
gsudo winget install Nushell.Nushell
```

```
Install-Module -Name PSReadLine -AllowPrerelease
mkdir -p (Get-Item $Profile.CurrentUserCurrentHost).DirectoryName
cp ./Microsoft.PowerShell_profile.ps1 (Get-Item $Profile.CurrentUserCurrentHost).DirectoryName
Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser
```

```
New-Item -ItemType SymbolicLink -Path "$HOME\projects\dotfiles\.starship" -Target "$HOME\"
```


# Get and Set up dotfiles

```bash
mkdir -p ~/projects
mkdir -p ~/.config
git clone https://github.com/Naruto/dotfiles.git ~/projects/dotfiles
DOTFILES_PATH=~/projects/dotfiles
pushd ${DOTFILES_PATH}
ln -sfn ${DOTFILES_PATH}/.emacs.d ~/
ln -sfn ${DOTFILES_PATH}/.zshrc ~/
ln -sfn ${DOTFILES_PATH}/.zsh ~/
ln -sfn ${DOTFILES_PATH}/.tmux ~/
ln -sfn ${DOTFILES_PATH}/.tmux.conf.local ~/
ln -sfn ${DOTFILES_PATH}/.starship ~/
ln -sfn ${DOTFILES_PATH}/.config/lazygit ~/.config/
ln -sfn ${DOTFILES_PATH}/.config/yazi ~/.config/
popd
```

Add the below section to `~/.gitconfig` file

```init
[include]
    path = ~/projects/dotfiles/.gitconfig
```
