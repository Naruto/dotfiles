# Emacs Keybind
bindkey -e

# History
HISTFILE=$HOME/.zsh-history
HISTSIZE=1000000
SAVEHIST=1000000
setopt extended_history
function history-all { history -E 1}

setopt share_history

autoload -U compinit
compinit
# ���Ĥ����䴰
zstyle ':completion:*' list-colors di=34 fi=0

autoload -U colors
colors

# based by http://devel.aquahill.net/zsh/zshoptions

# ʣ���� zsh ��Ʊ���˻Ȥ����ʤ� history �ե�����˾�񤭤����ɲä���
setopt append_history

# ���ꤷ�����ޥ��̾���ʤ����ǥ��쥯�ȥ�̾�Ȱ��פ������ cd ����
setopt auto_cd

# �䴰���䤬ʣ��������ˡ�����ɽ������
setopt auto_list

# �䴰������Tab, Ctrl+I) ��Ϣ�Ǥ�������ǽ���䴰�����ư���䴰����
setopt auto_menu

# ���å����б��ʤɤ�ưŪ���䴰����
setopt auto_param_keys

# �ǥ��쥯�ȥ�̾���䴰�������� / ��ưŪ���ղä��������䴰��������
setopt auto_param_slash

# �Ǹ夬�ǥ��쥯�ȥ�̾�ǽ���äƤ����������� / ��ưŪ�˼�����
#setopt auto_remove_slash

# �����ڥ����Υץ�����Ʊ�����ޥ��̾��¹Ԥ������ϥꥸ�塼�ह��
setopt auto_resume

# �ӡ��ײ����Ĥ餵�ʤ��褦�ˤ���
setopt NO_beep

# {a-c} �� a b c ��Ÿ�����뵡ǽ��Ȥ���褦�ˤ���
setopt brace_ccl

# �������ޥ�ɤ� echo �� BSD �ߴ��ˤ���
#setopt bsd_echo

# ����ܥ�å���󥯤ϼ��Τ��ɤ��褦�ˤʤ�
#setopt chase_links

# ��¸�Υե�������񤭤��ʤ��褦�ˤ���
setopt clobber

# ���ޥ�ɤΥ��ڥ�����å��򤹤�
setopt correct

# ���ޥ�ɥ饤�����ƤΥ��ڥ�����å��򤹤�
#setopt correct_all

# =command �� command �Υѥ�̾��Ÿ������
setopt equals

# �ե�����̾�� #, ~, ^ �� 3 ʸ��������ɽ���Ȥ��ư���
setopt extended_glob

# zsh �γ��ϡ���λ�����ҥ��ȥ�ե�����˽񤭹���
#setopt extended_history

# Ctrl+S/Ctrl+Q �ˤ��ե������Ȥ�ʤ��褦�ˤ���
setopt NO_flow_control

# �ƥ��ޥ�ɤ��¹Ԥ����Ȥ��˥ѥ���ϥå���������
#setopt hash_cmds

# ľ����Ʊ�����ޥ�ɥ饤��ϥҥ��ȥ���ɲä��ʤ�
setopt hist_ignore_dups

# ���ޥ�ɥ饤�����Ƭ�����ڡ����ǻϤޤ���ҥ��ȥ���ɲä��ʤ�
setopt hist_ignore_space

# �ҥ��ȥ��ƤӽФ��Ƥ���¹Ԥ���֤˰�ö�Խ��Ǥ�����֤ˤʤ�
setopt hist_verify

# �����뤬��λ���Ƥ�΢����֤� HUP �����ʥ������ʤ��褦�ˤ���
setopt NO_hup

# Ctrl+D �ǤϽ�λ���ʤ��褦�ˤʤ��exit, logout �ʤɤ�Ȥ���
setopt ignore_eof

# ���ޥ�ɥ饤��Ǥ� # �ʹߤ򥳥��Ȥȸ��ʤ�
setopt interactive_comments

# auto_list ���䴰��������ǡ�ls -F �Τ褦�˥ե�����μ��̤�ޡ���ɽ��
setopt list_types

# �������ޥ�� jobs �ν��Ϥ�ǥե���Ȥ� jobs -l �ˤ���
setopt long_list_jobs

# ���ޥ�ɥ饤��ΰ����� --prefix=/usr �ʤɤ� = �ʹߤǤ��䴰�Ǥ���
setopt magic_equal_subst

# �᡼�륹�ס��� $MAIL ���ɤޤ�Ƥ������˥󥰤�ɽ������
#setopt mail_warning

# �ե�����̾��Ÿ���ǥǥ��쥯�ȥ�˥ޥå�������������� / ���ղä���
setopt mark_dirs

# �䴰���䤬ʣ�������������ɽ�� (auto_list) �����������˺ǽ�θ�����䴰����
#setopt menu_complete

# ʣ���Υ�����쥯�Ȥ�ѥ��פʤɡ�ɬ�פ˱����� tee �� cat �ε�ǽ���Ȥ���
setopt multios

# �ե�����̾��Ÿ���ǡ������ǤϤʤ�����Ū�˥����Ȥ����褦�ˤʤ�
setopt numeric_glob_sort

# ���ޥ��̾�� / ���ޤޤ�Ƥ���Ȥ� PATH ��Υ��֥ǥ��쥯�ȥ��õ��
#setopt path_dirs

# 8 �ӥå��ܤ��̤��褦�ˤʤꡢ���ܸ�Υե�����̾�ʤɤ򸫤��褦�ˤʤ�
setopt print_eightbit

# ����ͤ� 0 �ʳ��ξ�罪λ�����ɤ�ɽ������
#setopt print_exit_value

# �ǥ��쥯�ȥꥹ���å���Ʊ���ǥ��쥯�ȥ���ɲä��ʤ��褦�ˤʤ�
#setopt pushd_ignore_dups

# pushd ������ʤ��Ǽ¹Ԥ������ pushd $HOME �ȸ��ʤ����
#setopt pushd_to_home

# rm * �ʤɤκݡ����������ƤΥե������ä����ɤ����γ�ǧ���ʤ��褦�ˤʤ�
#setopt rm_star_silent

# rm_star_silent �εդǡ�10 �ô�ȿ�����ʤ��ʤꡢƬ����ޤ����֤�Ϳ������
setopt rm_star_wait

# for, repeat, select, if, function �ʤɤǴ�άʸˡ���Ȥ���褦�ˤʤ�
setopt short_loops

# �ǥե���Ȥ�ʣ���ԥ��ޥ�ɥ饤���Խ��ǤϤʤ��������Խ��⡼�ɤˤʤ�
#setopt single_line_zle

# ���ޥ�ɥ饤�󤬤ɤΤ褦��Ÿ������¹Ԥ��줿����ɽ������褦�ˤʤ�
#setopt xtrace

# ����Ȥ�
setopt prompt_subst

# ������Υץ������Ȥ������ͭ
setopt share_history

# history (fc -l) ���ޥ�ɤ�ҥ��ȥ�ꥹ�Ȥ����������
setopt hist_no_store

# ʸ���������˲��ԥ����ɤ�̵�����Ǥ�ɽ������
unsetopt promptcr

#���ԥڤλ�rprompt����ɽ������
setopt transient_rprompt

# cd -[tab] ��pushd
setopt autopushd

# �Ĥ��ɽ��
setopt list_packed

# no beep sound when complete list displayed
setopt noautoremoveslash

# no beep sound when complete list displayed
setopt nolistbeep

# Setting Prompt
export PROMPT='[$HOST %c]%(!.#.%%) '

# Setting alias
alias ls="ls -F --color=auto --show-control-char"
alias rm="rm -i"

export LANG="ja_JP.UTF-8"
export LC_ALL="ja_JP.UTF-8"
export EDITOR="emacs"
export PAGER="lv"
export MANPATH="/usr/local/man:/usr/local/share/man:/usr/share/man:$MANPATH"

export PATH="/usr/local/bin:$PATH"

# for E
export E_PREFIX='/usr/local'
export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"

# go language
export GOARCH=amd64
export GOOS=linux

# Java
export PATH=/opt/java/bin:$PATH
export JAVA_HOME=/opt/java
export JAVA_FONTS=/usr/share/fonts/TTF

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
export PATH=~/bin:$PATH

