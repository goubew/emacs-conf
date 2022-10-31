#!/bin/bash

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$script_dir"

function symlink_el_file() {
  local el_file="$1"
  local symlink_dest="$2"

  if [[ -L "$symlink_dest" ]]; then
    unlink "$symlink_dest"
  elif [[ -e "$symlink_dest" ]]; then
    mv "$symlink_dest" "${symlink_dest}.bak"
  fi

  ln -s "$(realpath $el_file)" "$symlink_dest"
}

mkdir -p "$HOME/.emacs.d/straight/repos"
mkdir -p "$HOME/.emacs.d/straight/versions"

declare -A el_symlinks
el_symlinks[init.el]="$HOME/.emacs.d/init.el"
el_symlinks[early-init.el]="$HOME/.emacs.d/early-init.el"
el_symlinks[default.el]="$HOME/.emacs.d/straight/versions/default.el"

for el_file in "${!el_symlinks[@]}"; do
  symlink_el_file "$el_file" "${el_symlinks[$el_file]}"
done

if type git &> /dev/null; then
  if [[ ! -d "$HOME/.emacs.d/straight/repos/straight.el/.git" ]]; then
    (
      cd "$HOME/.emacs.d/straight/repos"
      git clone https://github.com/raxod502/straight.el.git
    )
  fi
else
  echo "Install git and clone https://github.com/raxod502/straight.el.git into $HOME/.emacs.d/straight/repos"
fi
