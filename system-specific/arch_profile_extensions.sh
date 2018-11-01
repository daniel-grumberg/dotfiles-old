#! /usr/bin/bash

export PATH="$PATH:$HOME/.local/bin"

if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]
then
    exec startx
fi

