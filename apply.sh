#!/bin/bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

function safe_link() {
  local src="$1"
  local dest="$2"

  if [[ -L $src ]]; then
    unlink "$src"
  elif [[ -e $src ]]; then
    mv "$src" "${src}.bak"
    ln -s "$src" "$dest"
  fi
}

mkdir -p "$HOME/.emacs.d/extra-packages"
mkdir -p "$HOME/.saves"
mkdir -p "$HOME/org"

safe_link "${SCRIPT_DIR}/init.el" "$HOME/.emacs.d/init.el"
safe_link "${SCRIPT_DIR}/early-init.el" "$HOME/.emacs.d/early-init.el"
safe_link "${SCRIPT_DIR}/desert-light-theme.el" "$HOME/.emacs.d/desert-light-theme.el"
safe_link "${SCRIPT_DIR}/app-launcher.el" "$HOME/.emacs.d/extra-packages/app-launcher.el"
