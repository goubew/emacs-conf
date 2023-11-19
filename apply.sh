#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"

mkdir -p "$HOME/.emacs.d/packages"
mkdir -p "$HOME/.saves"

ln -s init.el                     "$HOME/.emacs.d/init.el"
ln -s early-init.el               "$HOME/.emacs.d/early-init.el"
ln -s my-solarized-light-theme.el "$HOME/.emacs.d/my-solarized-light-theme.el"
ln -s breadcrumb.el               "$HOME/.emacs.d/packages/breadcrumb.el"
