# Emacs Keybind
bindkey -e

# History
HISTFILE=$HOME/.zsh-history
HISTSIZE=1000000
SAVEHIST=1000000
setopt extended_history
function history-all { history -E 1}

setopt share_history

export PATH="/usr/local/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"

case ${OSTYPE} in
    darwin*)
    if [ -f $(brew --prefix)/etc/autojump.sh ]; then
        source $(brew --prefix)/etc/autojump.sh
    fi
    fpath=($(brew --prefix)/share/zsh-completions $fpath)
    fpath=($(brew --prefix)/share/zsh/site-functions $fpath)
 
    ;;
esac
autoload -U compinit
compinit -u
# complete with color
zstyle ':completion:*' list-colors di=34 fi=0

autoload -U colors
colors

# based by http://devel.aquahill.net/zsh/zshoptions
setopt append_history
setopt auto_cd
setopt auto_list
setopt auto_menu
setopt auto_param_keys
setopt auto_param_slash
setopt auto_remove_slash
setopt auto_resume
setopt NO_beep
# {a-c} -> a b c 
setopt brace_ccl
# not override exist file
setopt clobber
# check command spell
setopt correct
setopt equals
# regex char: #, ~, ^ 
setopt extended_glob
#setopt extended_history
# no Ctrl+S/Ctrl+Q flow control
setopt NO_flow_control
## hash command
#setopt hash_cmds
# ignore duplication command
setopt hist_ignore_dups
# history ignore prefix space command 
setopt hist_ignore_space
# history verify
setopt hist_verify
# No HUP signal jobs at exit shell
setopt NO_hup
# ignore Ctrl+D (use exit or logout)
setopt ignore_eof
setopt interactive_comments
# add file types at auto_list
setopt list_types
# jobs alias 'jobs -l'(long information)
setopt long_list_jobs
setopt magic_equal_subst
#setopt mail_warning
setopt mark_dirs
#setopt menu_complete
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
setopt share_history
# ignore history command(fc -l)
setopt hist_no_store
unsetopt promptcr
setopt transient_rprompt
# cd -[tab] is pushd
setopt autopushd
setopt list_packed
# no beep sound when complete list displayed
setopt noautoremoveslash
# no beep sound when complete list displayed
setopt nolistbeep
# AUTO_CD
#setopt AUTO_CD
#cdpath=(.. ~ ~/projects)
# Setting Prompt
export PROMPT='[$HOST %c]%(!.#.%%) '

# Setting alias
alias rm="rm -i"
alias less="less -R"
alias ag='ag --pager "less -R"'
alias rg="rg --pretty"
alias lg='lazygit'
if ! type "gh" > /dev/null; then
    eval "$(gh completion -s zsh)"
fi

export LANG="ja_JP.UTF-8"
export LC_ALL="ja_JP.UTF-8"
export EDITOR="emacs"
export PAGER="less"
export MANPATH="/usr/local/man:/usr/local/share/man:/usr/share/man:$MANPATH"

export PATH="/usr/local/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"
export PATH="~/bin:$PATH"
export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"

# cask
export PATH="${HOME}/.cask/bin:$PATH"

# depot tools
export DEPOT_TOOLS_PATH=${HOME}/projects/depot_tools
export PATH="${DEPOT_TOOLS_PATH}:$PATH"

case ${OSTYPE} in
    linux*)
        alias ls="ls -F --show-control-char --color=always"
	      alias open=xdg-open

        # for E
        export E_PREFIX='/usr/local'

        # go language
        export GOARCH=amd64
        export GOOS=linux

        # Java
        export JAVA_HOME=$(readlink -f /usr/bin/javac | sed "s:/bin/javac::")

        # android
        export ANDROID_NDK=/opt/android/android-ndk
        export ANDROID_SDK=/opt/android/sdk/
        export STUDIO_HOME=/opt/android/android-studio
        export ANDROID_NDK_ROOT=${ANDROID_NDK}
        export NDK_ROOT=${ANDROID_NDK}
        export ANDROID_SDK_ROOT=${ANDROID_SDK}
        export ANDROID_HOME=${ANDROID_SDK}
        export GRADLE_HOME=${STUDIO_HOME}/gradle/gradle-2.2.1
        export PATH=${ANDROID_NDK}:$PATH
        export PATH=$PATH:${ANDROID_SDK}/platform-tool
        export PATH=$PATH:${ANDROID_SDK}/tools
        export PATH=$PATH:${ANDROID_SDK}/build-tools/21.1.2
        export PATH=${STUDIO_HOME}/bin:${PATH}
        export PATH=${GRADLE_HOME}/bin:${PATH}
        ;;

    darwin*)
        alias ls="exa -F"

        # Android
        export JAVA_HOME=`/usr/libexec/java_home -v 1.8`
        export GRADLE_HOME=/usr/local/opt/gradle
        export ANDROID_NDK=/opt/ndk/android-ndk
        export PATH=${ANDROID_NDK}:$PATH
        export ANDROID_SDK=${HOME}/Library/Android/sdk
        export ANDROID_NDK_ROOT=${ANDROID_NDK}
        export NDK_ROOT=${ANDROID_NDK}
        export ANDROID_SDK_ROOT=${ANDROID_SDK}
        export ANDROID_HOME=${ANDROID_SDK}
        export ANDROID_NDK_HOME=${ANDROID_NDK}
        export GRADLE_HOME=/usr/local/opt/gradle
        export ANT_ROOT=/usr/local/opt/ant/bin
        export PATH=${ANDROID_SDK}/platform-tools:$PATH
        export PATH=${ANDROID_SDK}/tools:$PATH
        ;;
esac

# rbenv
export PATH="$HOME/.rbenv/bin:$PATH"
if type "rbenv" > /dev/null 2>&1; then
    eval "$(rbenv init -)"
fi

# go
export GOPATH=$HOME/go
export GOROOT=/usr/local/opt/go/libexec
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:$GOROOT/bin

# gnu global uses pygments
export GTAGSCONF=/usr/local/share/gtags/gtags.conf
export GTAGSLABEL=pygments

# zsh suggestion
if [ -f '~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh' ]; then source '~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh'; fi

# peco
function peco-history-selection() {
    BUFFER=$(history -n -r 1 | peco --query "$LBUFFER")
    CURSOR=$#BUFFER
    zle reset-prompt
}
zle -N peco-history-selection
bindkey '^R' peco-history-selection

function peco-src () {
  local selected_dir=$(ghq list -p | peco --query "$LBUFFER")
  if [ -n "$selected_dir" ]; then
    BUFFER="cd ${selected_dir}"
    zle accept-line
  fi
  zle clear-screen
}
zle -N peco-src
bindkey '^]' peco-src

# cdr
if [[ -n $(echo ${^fpath}/chpwd_recent_dirs(N)) && -n $(echo ${^fpath}/cdr(N)) ]]; then
  autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
  add-zsh-hook chpwd chpwd_recent_dirs
  zstyle ':completion:*' recent-dirs-insert both
  zstyle ':chpwd:*' recent-dirs-default true
  zstyle ':chpwd:*' recent-dirs-max 1000
  zstyle ':chpwd:*' recent-dirs-file "$HOME/.cache/chpwd-recent-dirs"
  
  # ### search a destination from cdr list
  function peco-get-destination-from-cdr() {
    cdr -l | \
    sed -e 's/^[[:digit:]]*[[:blank:]]*//' | \
    peco --query "$LBUFFER"
  }

  ### search a destination from cdr list and cd the destination
  function peco-cdr() {
    local destination="$(peco-get-destination-from-cdr)"
    if [ -n "$destination" ]; then
      BUFFER="cd $destination"
      zle accept-line
    else
      zle reset-prompt
    fi
  }
  zle -N peco-cdr
  bindkey '^u' peco-cdr
fi

# eval "$(anyenv init -)"
# anyenv
export PATH="$HOME/.anyenv/bin:$PATH"
if type "anyenv" > /dev/null 2>&1; then
    eval "$(anyenv init -)"
fi


# fastlane
[ -f ~/.fastlane/completions/completion.sh ] && source ~/.fastlane/completions/completion.sh

# less
if type "src-hilite-lesspipe.sh" > /dev/null 2>&1; then
    LESSPIPE=`which src-hilite-lesspipe.sh`
    export LESSOPEN="| ${LESSPIPE} %s"
    export LESS=' -R -X -F '
fi

# android ndk
export PATH=/opt/ndk/android-ndk:$PATH

# android sdk
export PATH=${HOME}/Library/Android/android/platform-tools:$PATH
export PATH=${HOME}/Library/Android/android/tools:$PATH

# ccache
export USE_CCACHE=1
export NDK_CCACHE=/usr/local/bin/ccache
export CCACHE_CPP2=yes
export CCACHE_COMPILERCHECK=content

# ranger
function ranger() {
    if [ -z "$RANGER_LEVEL" ]; then
        /usr/local/bin/ranger $@
    else
        exit
    fi
}
function ranger-cd {
    tempfile="$(mktemp -t tmp.XXXXXX)"
    ranger --choosedir="$tempfile" "${@:-$(pwd)}"
    test -f "$tempfile" &&
    if [ "$(cat -- "$tempfile")" != "$(echo -n `pwd`)" ]; then
        cd -- "$(cat "$tempfile")"
    fi  
    rm -f -- "$tempfile"
}
bindkey -s '^O' 'ranger-cd\n'

export PATH="/usr/local/opt/openssl@1.1/bin:$PATH"

