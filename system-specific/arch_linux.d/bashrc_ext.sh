#! /usr/bin/bash

# Disbale stupid bell in pagers
alias less="less -Q"
PAGER="less -Q"
MANPAGER="less -Q"

alias setclip="xclip -i -selection c"
alias getclip="xclip -selection c -o"
