{ config, pkgs, lib, ... }:

with lib;
let
  cfg = config.ebn.windowManager.xmonad;
  compiler = "ghc884";
  ebn-xmonad = pkgs.haskell.packages.${compiler}.callPackage
    ./package/ebn-xmonad/xmonad.nix { };
in {
  options.ebn.windowManager.xmonad = {
    enable = mkEnableOption "Enable ebn xmonad config";
  };

  config = mkIf cfg.enable {
    home = {
      packages = with pkgs; [ yad xdotool bottom feh xorg.xset ];

      file.".xmonad/xmobar.conf".source = ./package/ebn-xmonad/xmobar.conf;

      file.".xmonad/xmonad-x86_64-linux".source =
        "${ebn-xmonad}/bin/ebn-xmonad-x86_64-linux";

      file.".xmonad/build" = {
        text = "# This file stops xmonad from recompiling on restart";
        executable = true;
      };
    };
  };
}
