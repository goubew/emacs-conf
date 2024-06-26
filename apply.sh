#!/bin/bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

mkdir -p "$HOME/.emacs.d/packages"
mkdir -p "$HOME/.saves"
mkdir -p "$HOME/org"

ln -s "${SCRIPT_DIR}"/init.el                     "$HOME/.emacs.d/init.el"
ln -s "${SCRIPT_DIR}"/early-init.el               "$HOME/.emacs.d/early-init.el"
ln -s "${SCRIPT_DIR}"/desert-light-theme.el "$HOME/.emacs.d/desert-light-theme.el"
