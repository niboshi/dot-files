#!/usr/bin/env bash
set -eux

PYENV_REPO_DIR="$HOME/.pyenv"
if [ ! -d "$PYENV_REPO_DIR" ]; then
    git clone https://github.com/pyenv/pyenv.git "$PYENV_REPO_DIR"
fi

git -C "$PYENV_REPO_DIR" pull

mkdir -p $HOME/.bashrc.d
cat > $HOME/.bashrc.d/pyenv <<EOF
export PYENV_ROOT="$PYENV_REPO_DIR"
export PATH="\$PYENV_ROOT"/bin:"\$PATH"
if command -v pyenv 1>/dev/null 2>&1; then
   eval "\$(pyenv init -)"
fi
EOF

cat <<EOF | bash
pyenv install 3.7.2
EOF

echo OK
