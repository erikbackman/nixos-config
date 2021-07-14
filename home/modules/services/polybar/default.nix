{ config, lib, pkgs, ... }:

let
  cfg = config.ebn.services.polybar;
in
with lib;
{
  options.ebn.services.polybar = {
    enable = mkEnableOption "Enable ebn polybar";
    extraModules = mkOption {
      type = with types; listOf functionTo string;
      default = [];
    };
  };

  config = mkIf cfg.enable {
    services.polybar = {
      enable = true;
      package = pkgs.polybar.override {
        alsaSupport = true;
        pulseSupport = true;
        mpdSupport = true;
      };
      config = ./config/polybar;
      extraConfig = ''
        [module/xmonad]
        type = custom/script
        exec = ${pkgs.xmonad-log}/bin/xmonad-log
        tail = true

        [module/spotify]
        type = custom/script
        exec = ${pkgs.playerctl}/bin/playerctl --player=spotify metadata --format "{{ artist }} - {{ title }}"
        format-prefix = " "
        format-suffix = "  "
        click-left = ${pkgs.playerctl}/bin/playerctl --player=spotify play-pause
        format-prefix-foreground = "#5ab977"
      '';
      script = "polybar main &";
    };
  };
}