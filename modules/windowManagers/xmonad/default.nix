input@{ config, pkgs, lib, ... }:

with lib;
let
  cfg = config.windowManager.ebn.xmonad;
  ebn-xmonad = pkgs.ebn.ebn-xmonad;
  configDir = "xmonad/xmonad-x86_64-linux";
  
in {
  
  imports = [ ./xcompmgr.nix ];

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
      mode = "0774";
    };

    environment.systemPackages = with pkgs; [
      flameshot
      feh
      xorg.xset
      xmonad-log
      nitrogen
      xcompmgr
      j4-dmenu-desktop
    ];
    
    services = {
      xserver = {
        enable = true;

        displayManager = {
          defaultSession = "none+xmonad";
          lightdm.greeters.mini = {
            enable = false;
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
          xset r rate 500 33
        '';

        windowManager.xmonad.enable = true;
      };
      ebn.xcompmgr.enable = true;
    };
  };
}
