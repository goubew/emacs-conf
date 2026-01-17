#!/bin/bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

function safe_link() {
  local src="$1"
  local dest="$2"

  if [[ -L $dest ]]; then
    unlink "$dest"
  elif [[ -e $dest ]]; then
    mv "$dest" "${dest}.bak"
  fi
  ln -s "$src" "$dest"
}

mkdir -p "$HOME/.emacs.d/packages"
mkdir -p "$HOME/.emacs.d/funs"
mkdir -p "$HOME/.saves"
mkdir -p "$HOME/org"

safe_link "${SCRIPT_DIR}/init.el" "$HOME/.emacs.d/init.el"
safe_link "${SCRIPT_DIR}/early-init.el" "$HOME/.emacs.d/early-init.el"
safe_link "${SCRIPT_DIR}/packages.el" "$HOME/.emacs.d/packages.el"
safe_link "${SCRIPT_DIR}/lilypond" "$HOME/.emacs.d/lilypond"
safe_link "${SCRIPT_DIR}/packages" "$HOME/.emacs.d/packages"
safe_link "${SCRIPT_DIR}/funs" "$HOME/.emacs.d/funs"
safe_link "${SCRIPT_DIR}/my-charcoal-light-theme.el" \
          "$HOME/.emacs.d/my-charcoal-light-theme.el"
safe_link "${SCRIPT_DIR}/my-gruvbox-light-theme.el" \
          "$HOME/.emacs.d/my-gruvbox-light-theme.el"
safe_link "${SCRIPT_DIR}/my-gruvbox-dark-theme.el" \
          "$HOME/.emacs.d/my-gruvbox-dark-theme.el"
