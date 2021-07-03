{ config, pkgs, lib, ... }:

with lib;
let
  cfg = config.ebn.windowManagers.xmonad;
  compiler = "ghc884";
  xmonadBin =
    pkgs.haskell.packages.${compiler}.callPackage ../../../config/xmonad { };
in {
  options.ebn.windowManagers.xmonad = {
    enable = mkEnableOption "Enable ebn xmonad config";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ ];
    home.file.".xmonad/ebn-xmonad".source = xmonadBin;
    home.file.".xmonad/xmobar.conf".source = ../../../config/xmonad/xmobar.conf;
    home.file.".xmonad/build" = {
      text = ''ln -s ./ebn-xmonad/bin/xmonad-x86_64-linux ./ -f'';
      executable = true;
    };
  };
}
