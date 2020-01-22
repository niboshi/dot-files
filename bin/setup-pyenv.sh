#!/usr/bin/env bash
set -eux

PYENV_REPO_DIR="$HOME/.pyenv"

#-----------------------------------
# Clone pyenv git repo
#-----------------------------------

if [ ! -d "$PYENV_REPO_DIR" ]; then
    git clone https://github.com/pyenv/pyenv.git "$PYENV_REPO_DIR"
fi

git -C "$PYENV_REPO_DIR" pull

#-----------------------------------
# Install Python build dependencies
#-----------------------------------
apt_pkgs=(
    libbz2-dev
    libreadline-dev
    libssl-dev
)

num_not_installed="$(
    dpkg-query --show --showformat='${db:Status-Status}\n' "${apt_pkgs[@]}" |
        grep not-installed |
        wc -l)"
if [ 0 != "$num_not_installed" ]; then
    sudo apt-get install "${apt_pkgs[@]}"
fi

#-----------------------------------
# Install Python in pyenv
#-----------------------------------
temp_init_file="$(mktemp)"

cat > "$temp_init_file" <<EOF
export PYENV_ROOT="$PYENV_REPO_DIR"
export PATH="\$PYENV_ROOT"/bin:"\$PATH"
if command -v pyenv 1>/dev/null 2>&1; then
   eval "\$(pyenv init -)"
fi
EOF

bash -eux -c '
source "'"$temp_init_file"'"
version=3.8.0
pyenv install "$version"
pyenv global "$version"
'

#-----------------------------------
# Postprocessing
#-----------------------------------
mkdir -p $HOME/.bash_profile.d
cp "$temp_init_file" $HOME/.bash_profile.d/pyenv

echo OK

set +x
echo "Type the following to make pyenv available immediately:"
echo "=============================================="
echo source $HOME/.bash_profile.d/pyenv
echo "=============================================="
