#! /bin/bash

set -e

function canonicalize {
    local TARGET_FILE=$(which $0)
    # Check if we got something in the path, if not use $0 as a starting point
    [[ -z "${TARGET_FILE}" ]] && TARGET_FILE="$0"

    cd $(dirname ${TARGET_FILE})
    TARGET_FILE=$(basename ${TARGET_FILE})

    # Iterate down a (possible) chain of symlinks
    while [[ -L "${TARGET_FILE}" ]]
    do
        TARGET_FILE=$(readlink ${TARGET_FILE})
        cd $(dirname ${TARGET_FILE})
        TARGET_FILE=$(basename ${TARGET_FILE})
    done

    # Compute the canonicalized name by finding the physical path
    # for the directory we're in and appending the target file.
    local PHYS_DIR=$(pwd -P)
    echo "${PHYS_DIR}/${TARGET_FILE}"
}

