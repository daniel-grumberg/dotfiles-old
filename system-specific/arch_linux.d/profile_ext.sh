#! /usr/bin/bash

rm -f "$HOME/.bash_history"

export PATH="$PATH:$HOME/.local/bin"
export VAGRANT_DEFAULT_PROVIDER="libvirt"

if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]
then
    exec startx
fi

