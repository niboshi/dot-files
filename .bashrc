#!/bin/bash

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac


shopt -s checkwinsize    # Update LINES and COLUMNS after each command execution
shopt -s globstar        # Allows recursive glob with **

# Disable terminal flow control (C-s to freeze output)
tty -s && stty -ixon

#---------------------------
# Key binding
#---------------------------
# Bind Alt-h to delete a preceding word
bind '"\eh": backward-kill-word'

#---------------------------
# history
#---------------------------
HISTSIZE=20000
HISTCONTROL=ignoreboth:erasedups
shopt -s histappend

#---------------------------
# terminal functions
# - colors
# - bold, italic, underline, strike
#---------------------------

term_reset() {
    echo -e "\e[0m"
}

term_fg_rgb() {
    local r="$1"
    local g="$2"
    local b="$3"
    local color=$((16 + $r * 36 + $g * 6 + $b))

    echo -e "\e[38;5;${color}m"
}

term_bg_rgb() {
    local r="$1"
    local g="$2"
    local b="$3"
    local color=$((16 + $r * 36 + $g * 6 + $b))

    echo -e "\e[48;5;${color}m"
}

term_fg_gray() {
    local level="$1"
    [ 0 -le "$level" -a "$level" -lt 24 ] || return 1
    echo -e "\e[38;5;$((232 + level))m"
}

term_bg_grayscale() {
    local level="$1"
    [ 0 -le "$level" -a "$level" -lt 24 ] || return 1
    echo -e "\e[48;5;$((232 + level))m"
}

term_fg_system() {
    local i="$1"
    [ 0 -le "$i" -a "$i" -lt 16 ] || return 1
    echo -e "\e[38;5;${i}m"
}

term_bg_system() {
    local i="$1"
    [ 0 -le "$i" -a "$i" -lt 16 ] || return 1
    echo -e "\e[48;5;${i}m"
}

term_bold() {
    echo -e "\e[1m"
}

term_italic() {
    echo -e "\e[3m"
}

term_underline() {
    echo -e "\e[4m"
}

term_strike() {
    echo -e "\e[9m"
}

_define_system_color_funcs() {
    local newline=$'\n'
    local name
    local line1
    local line2
    local line3
    local i
    local system_colors=(black red green yellow blue magenta cyan white)

    for i in $(seq 0 $((${#system_colors[@]}-1))); do
        name="${system_colors[i]}"

        # foreground
        line1='term_fg_'${name}'() {'
        line2='echo "$(term_fg_system '$i')"'
        line3='}'
        eval "${line1}${newline}${line2}${newline}${line3}"

        # background
        line1='term_bg_'${name}'() {'
        line2='echo "$(term_bg_system '$i')"'
        line3='}'
        eval "${line1}${newline}${line2}${newline}${line3}"

        # foreground (light)
        line1='term_fg_light_'${name}'() {'
        line2='echo "$(term_fg_system '$((8+i))')"'
        line3='}'
        eval "${line1}${newline}${line2}${newline}${line3}"

        # background (light)
        line1='term_bg_light_'${name}'() {'
        line2='echo "$(term_bg_system '$((8+i))')"'
        line3='}'
        eval "${line1}${newline}${line2}${newline}${line3}"
    done
}

_define_system_color_funcs
unset -f _define_system_color_funcs


#---------------------------
# prompt
#---------------------------
_niboshi_prompt_envs() {
    if [ ${#niboshi_envs[@]} -gt 0 ]; then
        local col_bracket="$(term_fg_rgb 1 1 1)"
        local col_env="$(term_fg_rgb 5 1 0)"
        echo -n "${col_bracket}[${col_env}"
        echo -n "${niboshi_envs[@]}"
        echo -n "${col_bracket}]"
    fi
}
_niboshi_prompt_tmux() {
    if [ -z "$TMUX" ]; then
        local col_bracket="$(term_fg_rgb 1 1 1)"
        local col_attached="$(term_fg_rgb 0 5 0)"
        local col_detached="$(term_fg_rgb 5 0 0)"

        local lines=()
        while read line; do if [ "${#line}" -gt 0 ]; then lines+=("$line"); fi; done <<< "$(tmux ls 2>/dev/null)"

        if [ "${#lines[@]}" -gt 0 ]; then
            echo -n "${col_bracket}(tmux"

            local i=0
            while [ $i -lt "${#lines[@]}" ]; do
                local line="${lines[i]}"
                local session_name="$(echo "$line" | sed -r 's/([^:]+):.*/\1/')"
                local is_attached=$(echo "$line" | grep -E '\(attached\)$' | wc -l)
                local col

                if [ "$is_attached" == 1 ]; then
                    col="$col_attached"
                else
                    col="$col_detached"
                fi

                echo -n " ${col}${session_name}"

                i=$((i + 1))
            done

            echo -n "${col_bracket})"
        fi
    fi
}

_set_prompt() {
    case "$HOSTNAME" in
        amane)
            host_color=green
            ;;
        natsumi)
            host_color=cyan
            ;;
        coyote)
            host_color=blue
            ;;
        pi)
            host_color=red
            ;;
        *)
            host_color=white
            ;;
    esac

    local host_color_expr='\[$(term_fg_'${host_color}')\]'
    PROMPT_COMMAND='hasjobs=$(jobs -p)'

    local platform
    if [ 1 == "$NIBOSHI_IS_CYGWIN" ]; then
        platform="$(term_fg_yellow)(cygwin)$(term_reset)"
    else
        platform=
    fi

    local line1="\[$(term_fg_red)\]:\$(_niboshi_prompt_envs)\$(_niboshi_prompt_tmux)\[$(term_fg_magenta)\]\[$(term_bold)\]\w\[$(term_reset)\]"
    local line2="[${host_color_expr}\[$(term_bold)\]\u\[$(term_reset)\]@${host_color_expr}\[$(term_bold)\]\h\[$(term_reset)\]]$platform"'${hasjobs:+$(term_fg_blue)(\j jobs)$(term_reset)}'"\$ "
    PS1="${line1}\n${line2}"

    export host_color
}

_set_prompt
unset -f _set_prompt

#---------------------------
# bashrc.d
#---------------------------
if [ -d "$HOME/.bashrc.d" ]; then
    for rcfile in $(find $HOME/.bashrc.d -maxdepth 1 ! -path . ! -name ".*" ! -name "_*"); do
        source "$rcfile"
    done
fi

unset rcfile

#---------------------------
# aliases
#---------------------------
alias less='less -S'
alias mysql='mysql --pager="less -S"'
alias ls='ls --color=auto'
alias PAGER=less

alias grep='/bin/grep --color=auto'
alias utf8='nkf --utf8'

#---------------------------
# LS_COLORS
#---------------------------
extlist="jpg|jpeg|gif|bmp|pbm|pgm|ppm|tga|xbm|xpm|tif|tiff|png|svg|mng|pcx|mov|mpg|mpeg|m2v|mkv|ogm|mp4|m4v|mp4v|vob|qt|nuv|wmv|asf|rm|rmvb|flc|avi|fli|gl|dl|xcf|xwd|yuv|aac|au|flac|mid|midi|mka|mp3|mpc|ogg|ra|wav|flv"
LS_COLORS_OLD=$LS_COLORS
LS_COLORS=$(echo $LS_COLORS | sed -re "s/:\*\.(${extlist})=[0-9].;[0-9].//g")
LS_COLORS=$(echo $LS_COLORS | sed -re "s/(^|:)ow=[^:]+(:|$)/\1ow=01;42\2/")

unset extlist

#---------------------------
