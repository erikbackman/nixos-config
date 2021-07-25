{ config, pkgs, lib, ... }:

with lib;
let
  cfg = config.xsession.ebn.windowManager.xmonad;
  ebn-xmonad = pkgs.ebn.ebn-xmonad;
in {
  options.xsession.ebn.windowManager.xmonad = {
    enable = mkEnableOption "Enable ebn xmonad config";
  };

  config = mkIf cfg.enable {
    home = {
      packages = with pkgs; [
        yad
        xdotool
        bottom
        flameshot
        feh
        xorg.xset
        xmonad-log
      ];

      file.".xmonad/xmonad-x86_64-linux".source =
        "${ebn-xmonad}/bin/ebn-xmonad-x86_64-linux";

      file.".xmonad/build" = {
        text = "# This file stops xmonad from recompiling on restart";
        executable = true;
      };

    };
    xsession = {
      enable = true;
      windowManager.xmonad.enable = true;
      initExtra = ''
          xrandr --output DP-0 --mode 3440x1440 --rate 99.98
          ~/.fehbg &
          xset r rate 500 33
          systemctl --user restart polybar

          if test -e $HOME/wallpaper; then
             ${pkgs.feh}/bin/feh --no-fehbg --bg-scale $HOME/wallpaper
          fi
      '';
    };
  };
}
