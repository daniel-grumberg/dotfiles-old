#!/bin/bash
function color_my_prompt {
    local __host="\[\033[01;32m\]@\h"
    local __cur_location="\[\033[01;34m\]\w"
    local __git_branch_color="\[\033[31m\]"
    local __git_branch='`git branch 2> /dev/null | grep -e ^* | sed -E  s/^\\\\\*\\(.+\)$/\\\\\1\ /`'
    local __prompt_tail="\[\033[35m\]\u$"
    local __last_color="\[\033[00m\]"
    export PS1="$__host $__cur_location$__git_branch_color$__git_branch\n$__prompt_tail$__last_color "
}

alias setclip="xclip -selection c"
alias getclip="xclip -selection c -o"

export EDITOR=vim
export VISUAL=vim

export ENV_SETUP=true

color_my_prompt

#Fix for the shitty colors for gruvbox...
source .vim/bundle/gruvbox/gruvbox_256palette.sh
