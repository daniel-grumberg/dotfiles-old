#! /bin/bash

if [[ -x ${1} ]]
then
    UTILS="$1"
else
    echo "This was not passed the right path to utils"
    exit 1
fi

source "$UTILS"

ARCH_DIR=$(dirname -- $(realpath "$(canonicalize)"))

ln -Tsnf "${ARCH_DIR}/xinitrc" "$HOME/.xinitrc"
[[ -d "$HOME/.Xresources.d" ]] && rm -rf "$HOME/.Xresources.d"
ln -snf "${ARCH_DIR}/Xresources.d" "$HOME/.Xresources.d"
ln -Tsnf "${ARCH_DIR}/Xresources" "$HOME/.Xresources"
ln -Tsnf "${ARCH_DIR}/Xmodmap" "$HOME/.Xmodmap"
[[ -d "$HOME/.local" ]] && rm -rf "$HOME/.local"
ln -snf "${ARCH_DIR}/local" "$HOME/.local"
[[ -d "$HOME/.config" ]] && rm -rf "$HOME/.config"
ln -snf "${ARCH_DIR}/config" "$HOME/.config"

echo "source ${ARCH_DIR}/profile_ext.sh" >> ~/.bash_profile
echo "source ${ARCH_DIR}/bashrc_ext.sh" >> ~/.bashrc


