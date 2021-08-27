sudo nixos-rebuild $1 --flake .#$2

find ./hm-modules -name install.sh -exec bash {} \;
