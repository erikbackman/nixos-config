{ config, pkgs, lib, ... }:

with lib;
let
  cfg = config.ebn.windowManager.xmonad;
  compiler = "ghc884";
  package =
    pkgs.haskell.packages.${compiler}.callPackage ./package/xmonad/xmonad.nix { };
in {
  options.ebn.windowManager.xmonad = {
    enable = mkEnableOption "Enable ebn xmonad config";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ ];
    home.file.".xmonad/ebn-xmonad".source = package;
    home.file.".xmonad/xmobar.conf".source = ./package/xmonad/xmobar.conf;
    home.file.".xmonad/build" = {
      text = ''ln -s ./ebn-xmonad/bin/xmonad-x86_64-linux ./ -f'';
      executable = true;
    };
  };
}
