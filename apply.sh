#!/bin/bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

mkdir -p "$HOME/.emacs.d/packages"
mkdir -p "$HOME/.saves"

ln -s "${SCRIPT_DIR}"/init.el                     "$HOME/.emacs.d/init.el"
ln -s "${SCRIPT_DIR}"/early-init.el               "$HOME/.emacs.d/early-init.el"
ln -s "${SCRIPT_DIR}"/my-solarized-light-theme.el "$HOME/.emacs.d/my-solarized-light-theme.el"
