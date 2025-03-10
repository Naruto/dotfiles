# What's this?

This is naruto's dotfiles.

The dotfiles aims to provide easily and smoothly operaions and recognizable syntax eye-candy in zsh of a terminal.

# Prepare

Install commands that are necessary to setup the dotfiles.

## macOS

```bash
brew install git emacs tmux zsh eza bat less moar
brew install cmake ghq gh fzf git-delta rg fd gradle yazi lazygit
brew install starship zoxide mcfly rustup-init mdcat htop
brew install sd hexyl
$(brew --prefix)/opt/fzf/install
rustup-init
```

## Windows

```cmd
winget install gsudo
gsudo winget install Nushell.Nushell
```

```powershell
Install-Module -Name PSReadLine -AllowPrerelease
mkdir -p (Get-Item $Profile.CurrentUserCurrentHost).DirectoryName
cp ./Microsoft.PowerShell_profile.ps1 (Get-Item $Profile.CurrentUserCurrentHost).DirectoryName
Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser
```

```powershell
New-Item -ItemType SymbolicLink -Path "$HOME\projects\dotfiles\.starship" -Target "$HOME\"
```

# Clone the repository and Setup dotfiles

```bash
mkdir -p ~/projects
mkdir -p ~/.config
git clone --recursive https://github.com/Naruto/dotfiles.git ~/projects/dotfiles
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
ln -sfn ${DOTFILES_PATH}/.config/bat ~/.config/
popd
```

Prepend the below section to `~/.gitconfig` file

```ini
[include]
    path = ~/projects/dotfiles/.gitconfig
```

Execute the below commands in zsh shell.

```bash
abbr import-aliases
abbr g co=checkout
abbr g st=status
abbr g di=diff
abbr g dt="difftool -y"
abbr g cm="commit -m"
abbr g br=branch
abbr g sm=submodule
ya pack -a yazi-rs/plugins:toggle-pane
```
