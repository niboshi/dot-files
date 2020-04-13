export LANG=C.UTF-8
export LC_ALL=C.UTF-8

export PYTHONDONTWRITEBYTECODE=1

if [ ! -z "$DISPLAY" ]; then
    setxkbmap -layout us -option ctrl:nocaps
fi

# Enable touchpanel in Firefox
export MOZ_USE_XINPUT2=1

#---------------------------
# PATH
#---------------------------
export PATH
PATH="$HOME/bin:$PATH"
PATH="$HOME/local/bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
for bin in $(find ~/.bindirs -mindepth 1 -maxdepth 1 2>/dev/null | sort -r); do
    PATH="$bin:$PATH"
done

unset bin
