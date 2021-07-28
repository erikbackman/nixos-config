{ config, pkgs, lib, ... }:

with lib;
let
  cfg = config.xsession.windowManager.ebn.xmonad;
  ebn-xmonad = pkgs.ebn.ebn-xmonad;
in {
  options.xsession.windowManager.ebn.xmonad = {
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

      file.".config/bottom/bottom.toml".text = ''
        [colors]
        border_color="#434C5E"
        highlighted_border_color="#bd93f9"
        table_header_color="green"
        widget_title_color="magenta"
      '';

    };
    xsession = {
      enable = true;
      windowManager.xmonad.enable = true;
      initExtra = ''
          xrandr --output DP-0 --mode 3440x1440 --rate 99.98
          xset r rate 500 33
          systemctl --user restart polybar

          if test -e $HOME/wallpaper; then
             ${pkgs.feh}/bin/feh --no-fehbg --bg-scale $HOME/wallpaper
          fi
      '';
    };
  };
}
