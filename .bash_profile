if [ -f ~/.profile ]; then
    source ~/.profile
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
