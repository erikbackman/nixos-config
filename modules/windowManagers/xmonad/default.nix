{ config, pkgs, lib, ... }:

with lib;
let
  cfg = config.windowManager.ebn.xmonad;
  ebn-xmonad = pkgs.ebn.ebn-xmonad;
  configDir = "xmonad/xmonad-x86_64-linux";
in {
  options.windowManager.ebn.xmonad = {
    enable = mkEnableOption "Enable ebn xmonad config";
  };

  config = mkIf cfg.enable {

    environment.variables = {
      "XMONAD_CONFIG_DIR" = "/etc/xmonad";
      "XMONAD_DATA_DIR" = "/etc/xmonad";
      "XMONAD_CACHE_DIR" = "/etc/xmonad";
    };

    environment.etc."${configDir}".source =
      "${ebn-xmonad}/bin/ebn-xmonad-x86_64-linux";

    environment.etc."xmonad/build" = {
      text = "# This file stops xmonad from recompiling on restart";
      mode = "0744";
    };

    environment.systemPackages = with pkgs; [
        yad
        xdotool
        bottom
        flameshot
        feh
        xorg.xset
        xmonad-log
    ];
    
    services = {
      xserver = {
        enable = true;
        layout = "se";
        videoDrivers = [ "nvidia" ];

        displayManager = {
          defaultSession = "none+xmonad";
          lightdm.greeters.mini = {
            enable = true;
            user = "ebn";
            extraConfig = ''
              [greeter-theme]
              background-image = "";
              background-color = "#0C0F12"
              text-color = "#ff79c6"
              password-background-color = "#1E2029"
              window-color = "#181a23"
              border-color = "#bd93f9"
            '';
          };
        };

        displayManager.sessionCommands = ''
          xrandr --output DP-0 --mode 3440x1440 --rate 99.98
          xrdb $HOME/.Xresources
          xset r rate 500 33

          if test -e $HOME/wallpaper; then
             ${pkgs.feh}/bin/feh --no-fehbg --bg-scale $HOME/wallpaper
          fi
        '';

        windowManager.xmonad.enable = true;
      };
    };

  };
}
