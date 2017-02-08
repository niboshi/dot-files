export LANG=C.UTF-8
export LC_ALL=C.UTF-8

export PYTHONDONTWRITEBYTECODE=1

if [ ! -z "$DISPLAY" ]; then
    setxkbmap -layout us -option ctrl:nocaps
fi

if [ "$(uname -o)" == "Cygwin" ]; then
    export NIBOSHI_IS_CYGWIN=1
else
    export NIBOSHI_IS_CYGWIN=0
fi

#---------------------------
# PATH
#---------------------------
export PATH
PATH="$HOME/bin:$PATH"
PATH="$HOME/local/bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
for bin in $(find ~/.bindirs -mindepth 1 -maxdepth 1 2>/dev/null); do
    PATH="$bin:$PATH"
done

unset bin
