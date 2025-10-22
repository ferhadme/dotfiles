#!/usr/bin/bash

# Script for changing emacs profile by changing sym link
# Usage: ./emacs-profile.sh mode
# Where mode is one of vanilla, doom
# doom - https://www.github.com/hlissner/doom-emacs

set -e

usage() {
    echo "Usage: $0 mode
  mode:{vanilla|doom}"
}

if [[ $# -ne 1 ]]; then
    usage
    exit 1
fi

MODE=$1

if [[ $MODE != vanilla && $MODE != doom ]]; then
    usage
    exit 1
fi


ln -sfn ~/.$MODE.emacs.d ~/.emacs.d
