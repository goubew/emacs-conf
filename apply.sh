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

for file in *.el; do
  file_basename="$(basename $file)"
  safe_link "${SCRIPT_DIR}/${file}" "$HOME/.emacs.d/${file_basename}"
done

safe_link "${SCRIPT_DIR}/lilypond" "$HOME/.emacs.d/lilypond"
safe_link "${SCRIPT_DIR}/packages" "$HOME/.emacs.d/packages"
safe_link "${SCRIPT_DIR}/funs" "$HOME/.emacs.d/funs"
