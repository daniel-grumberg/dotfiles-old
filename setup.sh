#! /bin/sh

DOTFILES=$(dirname -- $(realpath "$0"))

if [ -z "$ENV_SETUP" ]; then
    echo "source "$DOTFILES"/bashrc" >> ~/.bashrc
    ln -s "$DOTFILES"/vim ~/.vim
    ln -s "$DOTFILES"/vimrc ~/.vimrc
    cp "$DOTFILES"/ssh_config ~/.ssh/config
fi
