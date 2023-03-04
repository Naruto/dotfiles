# homebrew
[ -f "/usr/local/bin/brew" ] && eval $(/usr/local/bin/brew shellenv)
[ -f "/opt/homebrew/bin/brew" ] && eval $(/opt/homebrew/bin/brew shellenv)

# Emacs Keybind
bindkey -e

# History
HISTFILE=${HOME}/.zsh-history
HISTSIZE=1000000
SAVEHIST=1000000
function history-all { history -E 1}

FPATH=${HOME}/.zfunc:$FPATH

[ -n "${HOMEBREW_PREFIX+1}" ] && [ -d "${HOMEBREW_PREFIX}/share/zsh-completions" ] && FPATH=${HOMEBREW_PREFIX}/share/zsh-completions:$FPATH
autoload -Uz compinit
compinit -u

autoload -U colors
colors

# zsh options
## changing cirectories
setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups

## completions
setopt always_to_end
setopt auto_list
setopt auto_menu
setopt auto_param_slash
setopt complete_in_word
unsetopt menu_complete
setopt complete_aliases
setopt list_packed
setopt list_types
zstyle ':completion:*' list-colors di=34 fi=0
setopt nolistbeep

## expansion and globbing
setopt extended_glob
setopt glob_dots

## history
setopt append_history
setopt extended_history
unsetopt hist_beep
setopt hist_expire_dups_first
setopt hist_find_no_dups
setopt hist_ignore_all_dups
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_no_store
setopt hist_reduce_blanks
setopt hist_save_no_dups
setopt hist_verify
setopt inc_append_history
setopt share_history

## input/output
unsetopt clobber
setopt correct
unsetopt correct_all
unsetopt flow_control
setopt interactive_comments
unsetopt mail_warning
setopt path_dirs
setopt rm_star_wait
setopt ignore_eof

## job control
setopt auto_resume
unsetopt bg_nice
unsetopt check_jobs
unsetopt hup
setopt long_list_jobs
setopt notify

## prompting
setopt prompt_subst
setopt nopromptcr

## Zle
unsetopt beep
setopt combining_chars
setopt emacs
unset zle_bracketed_paste


# export LANG="ja_JP.UTF-8"
# export LC_ALL="ja_JP.UTF-8"

if [ -n "${HOMEBREW_PREFIX+1}" ] ; then
  export LOCAL_PREFIX=${HOMEBREW_PREFIX}
else 
  export LOCAL_PREFIX="/usr/local"

  export PATH="${LOCAL_PREFIX}/bin:${PATH}"
  export PATH="${LOCAL_PREFIX}/sbin:${PATH}"
  export LD_LIBRARY_PATH="${LOCAL_PREFIX}/lib:$LD_LIBRARY_PATH"
  export PKG_CONFIG_PATH="${LOCAL_PREFIX}/lib/pkgconfig:$PKG_CONFIG_PATH"

  export MANPATH="${LOCAL_PREFIX}/man:${LOCAL_PREFIX}/share/man:/usr/share/man:$MANPATH"
fi

export PATH="${HOME}/bin:${PATH}"

# cask
export PATH="${HOME}/.cask/bin:${PATH}"

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
  # export MANPAGER="sh -c 'col -bx | bat -l man -p'"
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
    # Jetbrains Toolbox
    export PATH=~/Library/Application\ Support/JetBrains/Toolbox/scripts:${PATH}

    # Android
    export JAVA_HOME=`/usr/libexec/java_home -v 17`
    export GRADLE_HOME=${LOCAL_PREFIX}/opt/gradle
    export ANDROID_NDK=/opt/ndk/android-ndk
    export PATH=${ANDROID_NDK}:${PATH}
    export ANDROID_SDK=${HOME}/Library/Android/sdk
    export ANDROID_NDK_ROOT=${ANDROID_NDK}
    export NDK_ROOT=${ANDROID_NDK}
    export ANDROID_SDK_ROOT=${ANDROID_SDK}
    export ANDROID_HOME=${ANDROID_SDK}
    export ANDROID_NDK_HOME=${ANDROID_NDK}
    export ANT_ROOT=${LOCAL_PREFIX}/opt/ant/bin
    export PATH=${ANDROID_SDK}/platform-tools:${PATH}
    export PATH=${ANDROID_SDK}/tools:${PATH}
    if [ -d ${ANDROID_SDK}/build-tools ]; then
      latest=$(/bin/ls ${ANDROID_SDK}/build-tools | sort -r | head -n 1)
      export PATH=${PATH}:${ANDROID_SDK}/build-tools/${latest}
    fi
    ;;
esac

# ccache
export USE_CCACHE=1
export NDK_CCACHE=${LOCAL_PREFIX}/bin/ccache
export CCACHE_CPP2=yes
export CCACHE_COMPILERCHECK=content


# rbenv
export PATH="${HOME}/.rbenv/bin:${PATH}"
if type "rbenv" > /dev/null 2>&1; then
  eval "$(rbenv init -)"
fi

# go
export GOPATH=${HOME}/go
export GOROOT=${LOCAL_PREFIX}/opt/go/libexec
export PATH=${PATH}:${GOPATH}/bin
export PATH=${PATH}:${GOROOT}/bin

# rust
export PATH=${HOME}/.cargo/bin:${PATH}

# zsh suggestion
[ -f "${HOME}/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh" ] && . "${HOME}/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh"

# zsh syntax highlighting
[ -f "${HOME}/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" ] && . "${HOME}/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

# fzf
[ -f ${HOME}/.fzf.zsh ] && . ${HOME}/.fzf.zsh
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
  export FORGIT_PATH=$(dirname $(readlink ${(%):-%N}))/forgit
  export FORGIT_PATH_ZSH="${FORGIT_PATH}/forgit.plugin.zsh"
  [ -f "${FORGIT_PATH_ZSH}" ] && . "${FORGIT_PATH_ZSH}"
fi

# git-fuzzy
GIT_FUZZY_PATH=$(dirname $(readlink ${(%):-%N}))/git-fuzzy
export PATH=${GIT_FUZZY_PATH}/bin:${PATH}
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
[ -f ${HOME}/.fastlane/completions/completion.sh ] && . ${HOME}/.fastlane/completions/completion.sh

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
    #     NNN_TMPFILE="${XDG_CONFIG_HOME:-${HOME}/.config}/nnn/.lastd"
    # NOTE: NNN_TMPFILE is fixed, should not be modified
    export NNN_TMPFILE="${XDG_CONFIG_HOME:-${HOME}/.config}/nnn/.lastd"

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


# Google Cloud SDK
if [ -f "${HOME}/google-cloud-sdk/path.zsh.inc" ]; then . "${HOME}/google-cloud-sdk/path.zsh.inc"; fi
if [ -f "${HOME}/google-cloud-sdk/completion.zsh.inc" ]; then . "${HOME}/google-cloud-sdk/completion.zsh.inc"; fi
