typeset -U path PATH
typeset -U fpath FPATH
typeset -U manpath MANPATH

if [[ -x /opt/homebrew/bin/brew ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
elif [[ -x /usr/local/bin/brew ]]; then
  eval "$(/usr/local/bin/brew shellenv)"
fi

# Emacs Keybind
bindkey -e

# History
HISTFILE=${HOME}/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000
function history-all { history -E 1 }

fpath=(${HOME}/.zfunc $fpath)

[[ -v HOMEBREW_PREFIX && -d "${HOMEBREW_PREFIX}/share/zsh-completions" ]] && fpath=(${HOMEBREW_PREFIX}/share/zsh-completions $fpath)
autoload -Uz compinit

local dump_files=(~/.zcompdump(N.mh+24))
if [[ ${#dump_files} -gt 0 || ! -f ~/.zcompdump ]]; then
  compinit
else
  compinit -C
fi

if [[ -s ~/.zcompdump && (! -s ~/.zcompdump.zwc || ~/.zcompdump -nt ~/.zcompdump.zwc) ]]; then
  zcompile ~/.zcompdump
fi

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
zstyle ':completion:*' list-colors 'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34' 'su=41;30' 'sg=46;30' 'tw=42;30' 'ow=43;30'

zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
[[ ! -d ~/.zsh/cache ]] && mkdir -p ~/.zsh/cache

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

  path=("${LOCAL_PREFIX}/bin" "${LOCAL_PREFIX}/sbin" $path)
  export LD_LIBRARY_PATH="${LOCAL_PREFIX}/lib:$LD_LIBRARY_PATH"
  export PKG_CONFIG_PATH="${LOCAL_PREFIX}/lib/pkgconfig:$PKG_CONFIG_PATH"

  manpath=("${LOCAL_PREFIX}/man" "${LOCAL_PREFIX}/share/man" "/usr/share/man" $manpath)
fi

path=("${HOME}/bin" $path)

# cask
path=("${HOME}/.cask/bin" $path)

# Added by Antigravity
path=("${HOME}/.antigravity/antigravity/bin" $path)

# Setting Prompt
if (( $+commands[starship] )); then
  export STARSHIP_CONFIG=${HOME}/.starship/config.toml
  export STARSHIP_CACHE=${HOME}/.starship/cache
  eval "$(starship init zsh)"
else
  export PROMPT='[$HOST %c]%(!.#.%%) '
fi

# gh command
if (( $+commands[gh] )); then
  eval "$(gh completion -s zsh)"
fi

# Setting alias
alias rm="rm -i"
alias less="less -R"
if (( $+commands[eza] )); then
  alias ls="eza -F"

  alias l='eza -lbF --git'                                               # list, size, type, git
  alias ll='eza -lbGF --git'                                             # long list
  alias llm='eza -lbGd --git --sort=modified'                            # long list, modified date sort
  alias la='eza -lbhHigUmuSa --time-style=long-iso --git --color-scale'  # all list
  alias lx='eza -lbhHigUmuSa@ --time-style=long-iso --git --color-scale' # all + extended list
else
  alias ls="ls -F --show-control-char --color=always"
fi
if (( $+commands[rg] )); then
  alias rg="rg -p"
fi
if (( $+commands[ag] )); then
  alias ag='ag --pager less'
fi
if (( $+commands[lstr] )); then
  alias tree='lstr'
fi
if (( $+commands[agy] )); then
  export EDITOR="agy -w"
elif (( $+commands[code] )); then
  export EDITOR="code -w"
else
  export EDITOR="emacs"
fi
if (( $+commands[bat] )); then
  alias cat='bat'
fi
if (( $+commands[moor] )); then
  export MOOR='-wrap --colors auto -mousemode auto -no-linenumbers -no-statusbar -no-clear-on-exit -style nord'
  export PAGER='moor'
  export BAT_PAGER='moor'
else
  export PAGER="less"
  export BAT_PAGER="less -R"
fi
if (( $+commands[btm] )); then
  alias top="btm"
fi

# case ${OSTYPE} in
#  linux*)
#    alias open=xdg-open
#     # Java
#     export JAVA_HOME=$(readlink -f /usr/bin/javac | sed "s:/bin/javac::")
#     ;;
#  darwin*)
#      ;;
#esac

export XDG_CONFIG_HOME="${HOME}/.config"

# Jetbrains Toolbox
path=("${HOME}/Library/Application Support/JetBrains/Toolbox/scripts" $path)

# Android
export JAVA_HOME=$(/usr/libexec/java_home -v 17)
export GRADLE_HOME=${LOCAL_PREFIX}/opt/gradle
export ANDROID_NDK=/opt/ndk/android-ndk
export ANDROID_SDK=${HOME}/Library/Android/sdk
export ANDROID_NDK_ROOT=${ANDROID_NDK}
export NDK_ROOT=${ANDROID_NDK}
export ANDROID_SDK_ROOT=${ANDROID_SDK}
export ANDROID_HOME=${ANDROID_SDK}
export ANDROID_NDK_HOME=${ANDROID_NDK}

path=(${ANDROID_SDK}/tools ${ANDROID_SDK}/platform-tools ${ANDROID_NDK} $path)
if [[ -d ${ANDROID_SDK}/build-tools ]]; then
    local build_tools=(${ANDROID_SDK}/build-tools/*(/Nn))
    if [[ ${#build_tools} -gt 0 ]]; then
        path=($path ${build_tools[-1]})
    fi
fi

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
path=("${HOME}/.rbenv/bin" $path)
if (( $+commands[rbenv] )); then
  eval "$(rbenv init -)"
fi

# go
export GOPATH=${HOME}/go
export GOROOT=${LOCAL_PREFIX}/opt/go/libexec
path=("${GOPATH}/bin" "${GOROOT}/bin" $path)

# rust
path=("${HOME}/.cargo/bin" $path)

# zsh abbr
[[ -f "${HOME}/.zsh/zsh-abbr/zsh-abbr.plugin.zsh" ]] && source "${HOME}/.zsh/zsh-abbr/zsh-abbr.plugin.zsh"

# zsh suggestion
[[ -f "${HOME}/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh" ]] && source "${HOME}/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh"

# fast syntax highlighting
[[ -f "${HOME}/.zsh/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh" ]] && source "${HOME}/.zsh/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh"

# fzf
if (( $+commands[fzf] )); then
  source <(fzf --zsh)
  export FZF_DEFAULT_COMMAND='fd -HL --exclude ".git"'
  export FZF_CTRL_T_COMMAND='fd -HL --exclude ".git" --type f'
  export FZF_ALT_C_COMMAND='fd -HL --exclude ".git" --type d'
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
if (( $+commands[zoxide] )); then    
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
	if cwd="$(<"$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
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
function lg() {
    export LAZYGIT_NEW_DIR_FILE="${HOME}/.lazygit/newdir"

    lazygit "$@"

    if [[ -f "$LAZYGIT_NEW_DIR_FILE" ]]; then
            cd "$(<"$LAZYGIT_NEW_DIR_FILE")"
            /bin/rm -f "$LAZYGIT_NEW_DIR_FILE" > /dev/null
    fi
}

# 1password
if (( $+commands[op] )); then
    eval "$(op completion zsh)"; compdef _op op
fi

# Google Cloud SDK
if [[ -f "${HOME}/google-cloud-sdk/path.zsh.inc" ]]; then source "${HOME}/google-cloud-sdk/path.zsh.inc"; fi
if [[ -f "${HOME}/google-cloud-sdk/completion.zsh.inc" ]]; then source "${HOME}/google-cloud-sdk/completion.zsh.inc"; fi

# local .zshrc
if [[ -f "${HOME}/.zshrc.local" ]]; then source "${HOME}/.zshrc.local"; fi
