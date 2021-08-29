{ config, lib, pkgs, ... }:

let
  cfg = config.services.ebn.polybar;
in with lib; {
  options.services.ebn.polybar = {
    enable = mkEnableOption "Enable polybar";
  };

  config = mkIf cfg.enable {
    fonts.fonts = with pkgs; [ font-awesome siji jetbrains-mono ];

    environment.etc."polybar/polybarrc".source = ./config/polybarrc;

    systemd.user.services."polybar" = {
      wantedBy = [ "graphical.target" ];
      script = "polybar -c /etc/polybar/polybarrc &";
    };
    #services.polybar = {
    #  enable = true;
    #  package = pkgs.polybar.override {
    #    alsaSupport = true;
    #    pulseSupport = true;
    #    mpdSupport = true;
    #  };
    #  config = ./config/polybar;
    #  extraConfig = ''
    #    [module/xmonad]
    #    type = custom/script
    #    exec = ${pkgs.xmonad-log}/bin/xmonad-log
    #    tail = true
    #  '';
    #  script = "polybar main &";
    #};
  };
}
#[module/spotify]
#type = custom/script
#exec = ${pkgs.playerctl}/bin/playerctl --player=ncspot metadata --format "{{ artist }} - {{ title }}"
#format-prefix = " "
#format-suffix = "  "
#click-left = ${pkgs.playerctl}/bin/playerctl --player=ncspot play-pause
#format-prefix-foreground = "#5ab977"
