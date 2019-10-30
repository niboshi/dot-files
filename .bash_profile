if [ -f ~/.profile ]; then
    source ~/.profile
fi

#---------------------------
# mintty color
#---------------------------
if [ 1 == "$NIBOSHI_IS_CYGWIN" ]; then
    palette=(
        # black  red      green    yellow   blue     magenta  cyan     white
        000000   cd0000   00cd00   cdcd00   1e90ff   cd00cd   00cdcd   e5e5e5
        4c4c4c   ff0000   00ff00   ffff00   4682b4   ff00ff   00ffff   ffffff
    )

    for i in $(seq 0 $((${#palette[@]}-1))); do
        rgb=${palette[i]}
        echo -ne "\e]4;${i};#${rgb}\a"
    done

    unset i
    unset rgb
    unset palette
fi

#---------------------------
# .bash_profile.d
#---------------------------
if [ -d "$HOME/.bash_profile.d" ]; then
    for rcfile in $(find $HOME/.bash_profile.d -maxdepth 1 ! -path . ! -name ".*" ! -name "_*"); do
        source "$rcfile"
    done
fi

unset rcfile

#---------------------------

if [ -f ~/.bashrc ]; then
    source ~/.bashrc
fi
