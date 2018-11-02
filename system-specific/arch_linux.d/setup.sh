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

echo "source ${ARCH_DIR}/profile_ext.sh" >> ~/.bash_profile
echo "source ${ARCH_DIR}/bashrc_ext.sh" >> ~/.bashrc
