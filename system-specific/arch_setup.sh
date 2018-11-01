#! /bin/bash

if [[ -z ${1+x} ]] then
    if [[ ! -e ${1} ]] then
        echo "This was not passed the right path to dotfiles"
        exit 1
    fi
    DOTFILES="$1"
else
    echo "This was not passed the right path to dotfiles"
    exit 1
fi

echo "source ${DOTFILES}/system-specific/arch_profile_extensions.sh" >> ~/.bash_profile
echo "source ${DOTFILES}/system-specific/arch_bashrc_extensions.sh" >> ~/.bash_profile
