#!/bin/bash

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac


UNAME_O=$(uname -o)

HISTSIZE=20000
HISTCONTROL=ignoreboth:erasedups

shopt -s histappend
shopt -s checkwinsize
shopt -s globstar


HOST_COLOR=0
case "$HOSTNAME" in
    amane)
        HOST_COLOR=32 # green
        ;;
    natsumi)
        HOST_COLOR=36 # cyan
        ;;
    coyote)
        HOST_COLOR=34 # blue
        ;;
    pi)
        HOST_COLOR=31 # red
        ;;
    *)
        HOST_COLOR=37 # light gray
        ;;
esac
PS1="\[\033[1;31m\]:\[\033[1;35m\]\w\n\[\033[0m\][\[\033[1;${HOST_COLOR}m\]\u\[\033[0m\]@\[\033[1;${HOST_COLOR}m\]\h\[\033[0m\]]\$ "
unset HOST_COLOR

if echo "${UNAME_O}" | grep -qi "cygwin"; then
    echo -e "\e]P44488ff"
    echo -e "\e]Pc8866ff"
fi

PATH="$PATH:~/bin"
PATH="$PATH:~/local/bin"
for bin in $(find ~/.bindirs -mindepth 1 -maxdepth 1 2>/dev/null); do
    PATH="$PATH:$bin"
done

### bashrc.d
if [ -d "$HOME/.bashrc.d" ]; then
    for rcfile in $(find $HOME/.bashrc.d -maxdepth 1 ! -path . ! -name ".*" ! -name "_*"); do
        source "$rcfile"
    done
fi

### Aliases
alias less='less -S'
alias mysql='mysql --pager="less -S"'
alias ls='ls --color=auto'
alias PAGER=less

### LS_COLORS
extlist="jpg|jpeg|gif|bmp|pbm|pgm|ppm|tga|xbm|xpm|tif|tiff|png|svg|mng|pcx|mov|mpg|mpeg|m2v|mkv|ogm|mp4|m4v|mp4v|vob|qt|nuv|wmv|asf|rm|rmvb|flc|avi|fli|gl|dl|xcf|xwd|yuv|aac|au|flac|mid|midi|mka|mp3|mpc|ogg|ra|wav|flv"
LS_COLORS_OLD=$LS_COLORS
LS_COLORS=$(echo $LS_COLORS | sed -re "s/:\*\.(${extlist})=[0-9].;[0-9].//g")
LS_COLORS=$(echo $LS_COLORS | sed -re "s/(^|:)ow=[^:]+(:|$)/\1ow=01;42\2/")


alias grep='/bin/grep --color=auto'

alias utf8='nkf --utf8'

PYTHONDONTWRITEBYTECODE=1

tty -s && stty -ixon
