{ config, options, lib, ... }:

let 
  mkOpt = type: default: lib.mkOption { inherit type default; };
  themes = import ./themes.nix;
in
{
  options = with lib; with types; {
    colors = mkOpt attrs themes.aurora;
  };
}
