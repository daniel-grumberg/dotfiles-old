#! /bin/bash

set -e

# Needs to be duplicated because we need it here to make sure we are being
# called from the right place
function canonicalize {
    local TARGET_FILE="$(which "$0")"
    # Check if we got something in the path, if not use $0 as a starting point
    [[ -z "${TARGET_FILE}" ]] && TARGET_FILE="$0"

    cd $(dirname "${TARGET_FILE}")
    TARGET_FILE=$(basename "${TARGET_FILE}")

    # Iterate down a (possible) chain of symlinks
    while [[ -L "${TARGET_FILE}" ]]
    do
        TARGET_FILE=$(readlink "${TARGET_FILE}")
        cd $(dirname ${TARGET_FILE})
        TARGET_FILE=$(basename "${TARGET_FILE}")
    done

    # Compute the canonicalized name by finding the physical path
    # for the directory we're in and appending the target file.
    local PHYS_DIR=$(pwd -P)
    echo "${PHYS_DIR}/${TARGET_FILE}"
}

DOTFILES=$(dirname -- $(realpath "$(canonicalize)"))

if [[ -e "${DOTFILES}/setup.lock" ]]
then
    echo "This machine was already setup. This was determined using setup.lock."

    exit 1
fi

ln -Fs "${DOTFILES}/vim" ~/.vim
ln -Fs "${DOTFILES}/vimrc" ~/.vimrc
ln -Fs "${DOTFILES}/emacs.d" ~/.emacs.d

mkdir -p ~/.ssh
cat "${DOTFILES}/ssh_config" >> ~/.ssh/config

echo "source ${DOTFILES}/bashrc" >> ~/.bashrc

if [[ $(uname -r) =~ "ARCH" ]]
then
    "${DOTFILES}/system-specific/arch_linux.d/setup.sh" "${DOTFILES}/utils.sh"
fi

touch "${DOTFILES}/setup.lock"

