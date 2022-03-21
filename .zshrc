# homebrew
[ -f "/usr/local/bin/brew" ] && eval $(/usr/local/bin/brew shellenv)
[ -f "/opt/homebrew/bin/brew" ] && eval $(/opt/homebrew/bin/brew shellenv)

# Emacs Keybind
bindkey -e

# History
HISTFILE=$HOME/.zsh-history
HISTSIZE=1000000
SAVEHIST=1000000
setopt extended_history
function history-all { history -E 1}

setopt share_history
FPATH=$HOME/.zfunc:$FPATH

[ -n "${HOMEBREW_PREFIX+1}" ] && FPATH=${HOMEBREW_PREFIX}/share/zsh/site-functions:$FPATH
autoload -Uz compinit
compinit -u

# zstyle ':completion:*' menu true select=2
#setopt menu_complete
setopt completealiases
setopt auto_list
setopt auto_menu
setopt list_packed
setopt list_types

# complete with color
zstyle ':completion:*' list-colors di=34 fi=0

autoload -U colors
colors

# based by http://devel.aquahill.net/zsh/zshoptions
setopt auto_cd
setopt auto_param_keys
setopt auto_param_slash
setopt auto_remove_slash
setopt auto_resume
setopt NO_beep
# {a-c} -> a b c 
setopt brace_ccl
setopt clobber # not override exist file
setopt correct # check command spell
setopt equals
setopt extended_glob # regex char: #, ~, ^ 
setopt NO_flow_control # no Ctrl+S/Ctrl+Q flow control
## hash command
#setopt hash_cmds

setopt append_history
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_verify
setopt hist_no_store
setopt share_history
setopt hist_reduce_blanks
setopt hist_expand
#setopt extended_history

setopt NO_hup
setopt ignore_eof
setopt interactive_comments
setopt long_list_jobs
setopt magic_equal_subst
setopt mark_dirs
setopt multios
setopt numeric_glob_sort
#setopt path_dirs
# print 8bit
setopt print_eightbit
# print exit value
#setopt print_exit_value
#setopt pushd_ignore_dups
#setopt pushd_to_home
#setopt rm_star_silent
setopt rm_star_wait
setopt short_loops
#setopt single_line_zle
#setopt xtrace
setopt prompt_subst
unsetopt promptcr
setopt transient_rprompt
# cd -[tab] is pushd
setopt autopushd
# no beep sound when complete list displayed
setopt noautoremoveslash
# no beep sound when complete list displayed
setopt nolistbeep

export LANG="ja_JP.UTF-8"
export LC_ALL="ja_JP.UTF-8"

if [ -n "${HOMEBREW_PREFIX+1}" ] ; then
  export LOCAL_PREFIX=${HOMEBREW_PREFIX}
else 
  export LOCAL_PREFIX="/usr/local"

  export PATH="${LOCAL_PREFIX}/bin:$PATH"
  export PATH="${LOCAL_PREFIX}/sbin:$PATH"
  export LD_LIBRARY_PATH="${LOCAL_PREFIX}/lib:$LD_LIBRARY_PATH"
  export PKG_CONFIG_PATH="${LOCAL_PREFIX}/lib/pkgconfig:$PKG_CONFIG_PATH"

  export MANPATH="${LOCAL_PREFIX}/man:${LOCAL_PREFIX}/share/man:/usr/share/man:$MANPATH"
fi

export PATH="${HOME}/bin:$PATH"

# cask
export PATH="${HOME}/.cask/bin:$PATH"

# Setting Prompt
if type "starship" > /dev/null; then
  export STARSHIP_CONFIG=${HOME}/.starship/config.toml
  export STARSHIP_CACHE=${HOME}/.starship/cache
  eval "$(starship init zsh)"
else
  export PROMPT='[$HOST %c]%(!.#.%%) '
fi

# gh command
if type "gh" > /dev/null; then
  eval "$(gh completion -s zsh)"
fi

# Setting alias
alias rm="rm -i"
alias less="less -R"
alias ag='ag --pager "less -R"'
alias rg="rg --pretty"
alias lg='lazygit'
if type "exa" > /dev/null; then
  alias ls="exa -F"
  alias tree="exa -T"

  alias l='exa -lbF --git'                                                # list, size, type, git
  alias ll='exa -lbGF --git'                                             # long list
  alias llm='exa -lbGd --git --sort=modified'                            # long list, modified date sort
  alias la='exa -lbhHigUmuSa --time-style=long-iso --git --color-scale'  # all list
  alias lx='exa -lbhHigUmuSa@ --time-style=long-iso --git --color-scale' # all + extended list
else
  alias ls="ls -F --show-control-char --color=always"
fi
if type "bat" > /dev/null; then
  export BAT_PAGER="less -RF"
  alias cat="bat --paging=never --style=plain"
  export MANPAGER="sh -c 'col -bx | bat -l man -p'"
fi
if type htop > /dev/null; then
  alias top="htop"
fi

if type "code" > /dev/null; then
  export EDITOR="code -w"
else
  export EDITOR="emacs"
fi
export PAGER="less"


case ${OSTYPE} in
  linux*)
    alias open=xdg-open
     # Java
     export JAVA_HOME=$(readlink -f /usr/bin/javac | sed "s:/bin/javac::")
     ;;
  darwin*)
    # Android
    export JAVA_HOME=`/usr/libexec/java_home -v 11`
    export GRADLE_HOME=${LOCAL_PREFIX}/opt/gradle
    export ANDROID_NDK=/opt/ndk/android-ndk
    export PATH=${ANDROID_NDK}:$PATH
    export ANDROID_SDK=${HOME}/Library/Android/sdk
    export ANDROID_NDK_ROOT=${ANDROID_NDK}
    export NDK_ROOT=${ANDROID_NDK}
    export ANDROID_SDK_ROOT=${ANDROID_SDK}
    export ANDROID_HOME=${ANDROID_SDK}
    export ANDROID_NDK_HOME=${ANDROID_NDK}
    export ANT_ROOT=${LOCAL_PREFIX}/opt/ant/bin
    export PATH=${ANDROID_SDK}/platform-tools:$PATH
    export PATH=${ANDROID_SDK}/tools:$PATH
    ;;
esac

# ccache
export USE_CCACHE=1
export NDK_CCACHE=${LOCAL_PREFIX}/bin/ccache
export CCACHE_CPP2=yes
export CCACHE_COMPILERCHECK=content


# rbenv
export PATH="$HOME/.rbenv/bin:$PATH"
if type "rbenv" > /dev/null 2>&1; then
  eval "$(rbenv init -)"
fi

# go
export GOPATH=$HOME/go
export GOROOT=${LOCAL_PREFIX}/opt/go/libexec
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:$GOROOT/bin

# rust
export PATH=$HOME/.cargo/bin:$PATH

# zsh suggestion
[ -f "$HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh" ] && . "$HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh"

# zsh syntax highlighting
[ -f "$HOME/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" ] && . "$HOME/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

# fzf
[ -f ${HOME}/.fzf.zsh ] && source ${HOME}/.fzf.zsh
export FZF_DEFAULT_COMMAND='fd -HL --exclude ".git"'
export FZF_DEFAULT_OPTS='--layout=reverse --ansi --border --bind ctrl-v:page-down,alt-v:page-up,ctrl-k:kill-line'

## ghq and fzf
function ghq-fzf() {
  local target_dir=$(ghq list -p | fzf --query="$LBUFFER")

  if [ -n "$target_dir" ]; then
    BUFFER="cd ${target_dir}"
    zle accept-line
  fi

  zle reset-prompt
}
zle -N ghq-fzf
bindkey "^]" ghq-fzf

## git co branch and fzf
_fzf_complete_git() {
  ARGS="$@"
  local branches

  if [[ $ARGS == 'git co'* ]]; then
    branches=$(git branch -vv --all)
    _fzf_complete --reverse --multi -- "$@" < <(
      echo $branches
    )
  else
    eval "zle ${fzf_default_completion:-expand-or-complete}"
  fi
}
_fzf_complete_git_post() {
    awk '{print $1}'
}

# forgit
if type "fzf" > /dev/null; then
  FORGIT_PATH=$(dirname $(readlink ${(%):-%N}))/forgit
  FORGIT_PATH_ZSH="${FORGIT_PATH}/forgit.plugin.zsh"
  [ -f "${FORGIT_PATH_ZSH}" ] && source "${FORGIT_PATH_ZSH}"
fi

# git-fuzzy
GIT_FUZZY_PATH=$(dirname $(readlink ${(%):-%N}))/git-fuzzy
export PATH=${GIT_FUZZY_PATH}/bin:$PATH
if type "delta" > /dev/null; then
  export GF_PREFERRED_PAGER="delta --theme=gruvbox --highlight-removed -w __WIDTH__"
fi
if type "bat" > /dev/null; then
  export GF_BAT_STYLE=changes
  export GF_BAT_THEME=zenburn
fi

# zoxide
if type "zoxide" > /dev/null 2>&1; then    
    eval "$(zoxide init zsh)"

    function zoxide-fzf() {
      BUFFER="zi"
      zle accept-line
      zle reset-prompt
    }
    zle -N zoxide-fzf
    bindkey '^U' zoxide-fzf
fi

# fastlane
[ -f ${HOME}/.fastlane/completions/completion.sh ] && source ${HOME}/.fastlane/completions/completion.sh

# nnn
export NNN_PLUG="d:-_git diff;l:-_git log;s:-_git status;j:autojump"
function n()
{
    # Block nesting of nnn in subshells
    if [ -n $NNNLVL ] && [ "${NNNLVL:-0}" -ge 1 ]; then
        echo "nnn is already running"
        return
    fi

    # The default behaviour is to cd on quit (nnn checks if NNN_TMPFILE is set)
    # To cd on quit only on ^G, remove the "export" as in:
    #     NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"
    # NOTE: NNN_TMPFILE is fixed, should not be modified
    export NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"

    # Unmask ^Q (, ^V etc.) (if required, see `stty -a`) to Quit nnn
    # stty start undef
    # stty stop undef
    # stty lwrap undef
    # stty lnext undef

    nnn -deH "$@"

    if [ -f "$NNN_TMPFILE" ]; then
            . "$NNN_TMPFILE"
            rm -f "$NNN_TMPFILE" > /dev/null
    fi
}
function n_() {
    BUFFER="n"
    zle accept-line

    zle reset-prompt
 }
 zle -N n_
bindkey '^O' n_

# lazygit
function lg()
{
    export LAZYGIT_NEW_DIR_FILE=${HOME}/.lazygit/newdir

    lazygit "$@"

    if [ -f $LAZYGIT_NEW_DIR_FILE ]; then
            cd "$(cat $LAZYGIT_NEW_DIR_FILE)"
            rm -f $LAZYGIT_NEW_DIR_FILE > /dev/null
    fi
}

# 1password
if type "op" > /dev/null; then
    eval "$(op completion zsh)"; compdef _op op
fi
