export PYTHONDONTWRITEBYTECODE=1

#---------------------------
# PATH
#---------------------------
PATH="$PATH:~/bin"
PATH="$PATH:~/local/bin"
for bin in $(find ~/.bindirs -mindepth 1 -maxdepth 1 2>/dev/null); do
    PATH="$PATH:$bin"
done

unset bin
