# Emacs Keybind
bindkey -e

# History
HISTFILE=$HOME/.zsh-history
HISTSIZE=1000000
SAVEHIST=1000000
setopt extended_history
function history-all { history -E 1}

setopt share_history


case ${OSTYPE} in
    darwin*)
    if [ -f $(brew --prefix)/etc/autojump.sh ]; then
        source $(brew --prefix)/etc/autojump.sh
    fi
    fpath=($(brew --prefix)/share/zsh/site-functions $fpath)
 
    ;;
esac
autoload -U compinit
compinit
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

export LANG="ja_JP.UTF-8"
export LC_ALL="ja_JP.UTF-8"
export EDITOR="emacs"
export PAGER="lv"
export MANPATH="/usr/local/man:/usr/local/share/man:/usr/share/man:$MANPATH"

export PATH="/usr/local/bin:$PATH"
export PATH=~/bin:$PATH
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
        alias ls="ls -F"
        ;;
esac

# go
export GOPATH=$HOME/go
export PATH="$GOPATH/bin:$PATH"

# npm command completion script
COMP_WORDBREAKS=${COMP_WORDBREAKS/=/}
COMP_WORDBREAKS=${COMP_WORDBREAKS/@/}
export COMP_WORDBREAKS

if complete &>/dev/null; then
    _npm_completion () {
        local si="$IFS"
        IFS=$'\n' COMPREPLY=($(COMP_CWORD="$COMP_CWORD" \
            COMP_LINE="$COMP_LINE" \
            COMP_POINT="$COMP_POINT" \
            npm completion -- "${COMP_WORDS[@]}" \
            2>/dev/null)) || return $?
        IFS="$si"
    }
    complete -F _npm_completion npm
elif compctl &>/dev/null; then
    _npm_completion () {
        local cword line point words si
        read -Ac words
        read -cn cword
        let cword-=1
        read -l line
        read -ln point
        si="$IFS"
        IFS=$'\n' reply=($(COMP_CWORD="$cword" \
            COMP_LINE="$line" \
            COMP_POINT="$point" \
            npm completion -- "${words[@]}" \
            2>/dev/null)) || return $?
        IFS="$si"
    }
    compctl -K _npm_completion npm
fi

# percol
function exists { which $1 &> /dev/null }

if exists percol; then
    function percol_select_history() {
        local tac
        exists gtac && tac="gtac" || { exists tac && tac="tac" || { tac="tail -r" } }
        BUFFER=$(history -n 1 | eval $tac | percol --query "$LBUFFER")
        CURSOR=$#BUFFER         # move cursor
        zle -R -c               # refresh
    }

    zle -N percol_select_history
    bindkey '^R' percol_select_history
fi

# gnu global uses pygments
export GTAGSCONF=/usr/local/share/gtags/gtags.conf
export GTAGSLABEL=pygments

# zsh suggestion
#source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

# peco
function peco-history-selection() {
    local tac
    exists gtac && tac="gtac" || { exists tac && tac="tac" || { tac="tail -r" } }

    BUFFER=`history -n 1 | tac  | awk '!a[$0]++' | peco`
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

## powered_cd
function chpwd() {
  powered_cd_add_log
}

function powered_cd_add_log() {
  local i=0
  cat ~/.powered_cd.log | while read line; do
    (( i++ ))
    if [ i = 30 ]; then
      sed -i -e "30,30d" ~/.powered_cd.log
    elif [ "$line" = "$PWD" ]; then
      sed -i -e "${i},${i}d" ~/.powered_cd.log 
    fi
  done
  echo "$PWD" >> ~/.powered_cd.log
}

function powered_cd() {
  local tac
  exists gtac && tac="gtac" || { exists tac && tac="tac" || { tac="tail -r" } }
  if [ $# = 0 ]; then
    cd $(tac ~/.powered_cd.log | peco)
  elif [ $# = 1 ]; then
    cd $1
  else
    echo "powered_cd: too many arguments"
  fi
}

_powered_cd() {
  _files -/
}

compdef _powered_cd powered_cd
alias c="powered_cd"

export PATH="$HOME/.anyenv/bin:$PATH"
eval "$(anyenv init -)"
