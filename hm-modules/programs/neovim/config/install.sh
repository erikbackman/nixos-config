#!/usr/bin/env bash

set -eo pipefail

here=$(dirname "$0")
target=$(realpath "$here/lua/")
link="$HOME/.config/nvim/lua"

printf "creating symlink %s -> %s\n" $link $target
ln -sf $target $link
