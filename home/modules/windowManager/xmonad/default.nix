{ config, pkgs, lib, ... }:

with lib;
let
  cfg = config.ebn.windowManager.xmonad;
  compiler = "ghc884";
  ebn-xmonad = pkgs.haskell.packages.${compiler}.callPackage ./package/xmonad/xmonad.nix { };
in {
  options.ebn.windowManager.xmonad = {
    enable = mkEnableOption "Enable ebn xmonad config";
  };

  config = mkIf cfg.enable {
    home.packages = [ ebn-xmonad ];
    home.file.".xmonad/xmobar.conf".source = ./package/xmonad/xmobar.conf;
    home.file.".xmonad/build" = {
      text = ''ln -s $(which ebn-xmonad-x86_64-linux) ./xmonad-x86_64-linux -f'';
      executable = true;
    };
  };
}
