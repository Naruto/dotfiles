# homebrew
[[ -f "/usr/local/bin/brew" ]] && eval $(/usr/local/bin/brew shellenv)
[[ -f "/opt/homebrew/bin/brew" ]] && eval $(/opt/homebrew/bin/brew shellenv)

# Emacs Keybind
bindkey -e

# History
HISTFILE=${HOME}/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000
function history-all { history -E 1}

fpath=(${HOME}/.zfunc $fpath)

[[ -v HOMEBREW_PREFIX && -d "${HOMEBREW_PREFIX}/share/zsh-completions" ]] && fpath=(${HOMEBREW_PREFIX}/share/zsh-completions $fpath)
autoload -Uz compinit
for dump in ~/.zcompdump(N.mh+24); do
  compinit
done
compinit -C

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

if [[ -v HOMEBREW_PREFIX ]] ; then
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

# Added by Antigravity
export PATH="${HOME}/.antigravity/antigravity/bin:$PATH"

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
if type "eza" > /dev/null; then
  alias ls="eza -F"

  alias l='eza -lbF --git'                                                # list, size, type, git
  alias ll='eza -lbGF --git'                                             # long list
  alias llm='eza -lbGd --git --sort=modified'                            # long list, modified date sort
  alias la='eza -lbhHigUmuSa --time-style=long-iso --git --color-scale'  # all list
  alias lx='eza -lbhHigUmuSa@ --time-style=long-iso --git --color-scale' # all + extended list
else
  alias ls="ls -F --show-control-char --color=always"
fi
if type "rg" > /dev/null; then
  alias rg="rg -p"
fi
if type "ag" > /dev/null; then
  alias ag='ag --pager less'
fi
if type lstr > /dev/null; then
  alias tree='lstr'
fi
if type "agy" > /dev/null; then
  export EDITOR="agy -w"
elif type "code" > /dev/null; then
  export EDITOR="code -w"
else
  export EDITOR="emacs"
fi
if type "bat" > /dev/null; then
  alias cat='bat'
fi
if type "moor" > /dev/null 2>&1; then
  export MOOR='-wrap --colors auto -mousemode auto -no-linenumbers -no-statusbar -no-clear-on-exit -style nord'
  export PAGER='moor'
  export BAT_PAGER='moor'
else
  export PAGER="less"
  export BAT_PAGER="less -R"
fi
if type "btm" > /dev/null; then
  alias top="btm"
fi

case ${OSTYPE} in
  linux*)
    alias open=xdg-open
     # Java
     export JAVA_HOME=$(readlink -f /usr/bin/javac | sed "s:/bin/javac::")
     ;;
  darwin*)
    export XDG_CONFIG_HOME="${HOME}/.config"

    # Jetbrains Toolbox
    export PATH=${PATH}:~/Library/Application\ Support/JetBrains/Toolbox/scripts

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
    if [[ -d ${ANDROID_SDK}/build-tools ]]; then
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
# sccache
export SCCACHE_CACHE_SIZE=100G
export SCCACHE_DIR=~/.sccache
export SCCACHE_CACHE_MULTIARCH="1"

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

# zsh abbr
[[ -f "${HOME}/.zsh/zsh-abbr/zsh-abbr.plugin.zsh" ]] && source "${HOME}/.zsh/zsh-abbr/zsh-abbr.plugin.zsh"

# zsh suggestion
[[ -f "${HOME}/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh" ]] && source "${HOME}/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh"

# fast syntax highlighting
[[ -f "${HOME}/.zsh/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh" ]] && source "${HOME}/.zsh/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh"

# fzf
if type "fzf" > /dev/null 2>&1; then
  source <(fzf --zsh)
  export FZF_DEFAULT_COMMAND='fd -HL --exclude ".git"'
  export FZF_DEFAULT_OPTS='--layout=reverse --ansi --border --bind ctrl-v:page-down,alt-v:page-up,ctrl-k:kill-line'
fi

## ghq and fzf
function ghq-fzf() {
  local target_dir=$(ghq list -p | fzf --query="$LBUFFER")

  if [[ -n "$target_dir" ]]; then
    BUFFER="cd ${target_dir}"
    zle accept-line
  fi

  zle reset-prompt
}
zle -N ghq-fzf
bindkey "^]" ghq-fzf

# zoxide
if type "zoxide" > /dev/null 2>&1; then    
    eval "$(zoxide init zsh)"

    function zi_() {
      BUFFER="zi"
      zle accept-line
      zle reset-prompt
    }
    zle -N zi_
    bindkey '^U' zi_
fi

# # mcfly
# if type "mcfly" > /dev/null 2>&1; then   
#     eval "$(mcfly init zsh)"
#     export MCFLY_FUZZY=1
#     export MCFLY_RESULTS=50
#     export MCFLY_RESULTS_SORT=LAST_RUN
#     export MCFLY_PROMPT="➜"
# fi

# fastlane
[[ -f ${HOME}/.fastlane/completions/completion.sh ]] && source ${HOME}/.fastlane/completions/completion.sh

# yazi
function y() {
	local tmp="$(mktemp -t "yazi-cwd.XXXXXX")" cwd
	yazi --cwd-file="$tmp"
	if cwd="$(command cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
		builtin cd -- "$cwd"
	fi
	/bin/rm -f -- "$tmp"
}
function y_() {
    BUFFER="y"
    zle accept-line

    zle reset-prompt
}
zle -N y_
bindkey '^O' y_

# lazygit
function lg()
{
    export LAZYGIT_NEW_DIR_FILE=${HOME}/.lazygit/newdir

    lazygit "$@"

    if [[ -f $LAZYGIT_NEW_DIR_FILE ]]; then
            cd "$(/bin/cat $LAZYGIT_NEW_DIR_FILE)"
            /bin/rm -f $LAZYGIT_NEW_DIR_FILE > /dev/null
    fi
}

# 1password
if type "op" > /dev/null; then
    eval "$(op completion zsh)"; compdef _op op
fi


# Google Cloud SDK
if [[ -f "${HOME}/google-cloud-sdk/path.zsh.inc" ]]; then source "${HOME}/google-cloud-sdk/path.zsh.inc"; fi
if [[ -f "${HOME}/google-cloud-sdk/completion.zsh.inc" ]]; then source "${HOME}/google-cloud-sdk/completion.zsh.inc"; fi

