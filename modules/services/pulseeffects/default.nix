{ config, lib, pkgs, ... }:

let
  cfg = config.services.ebn.pulseeffects;
in with lib; {
  options.services.ebn.pulseeffects = {
    enable = mkEnableOption "Enable PulseEffects";
    package = mkOption {
      type = types.package;
      default = pkgs.pulseeffects-legacy;
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [
      cfg.package
      pkgs.at-spi2-core
      pkgs.gdk-pixbuf
    ];

    programs.dconf.enable = true;
    services.gnome.at-spi2-core.enable = true;
    systemd.user.services.pulseeffects = {
      description = "PulseEffects Daemon"; 
      requires = [ "dbus.service" ];
      wantedBy = [ "graphical-session.target" ];
      after = [ "graphical-session-pre.target" ];
      partOf = [ "graphical-session.target" "pulseaudio.service" ];
      serviceConfig = {
        ExecStart = "${pkgs.pulseeffects-legacy}/bin/pulseeffects --gapplication-service --load-preset ebn-preset";
        ExecStop = "${pkgs.pulseeffects-legacy}/bin/pulseeffects --quit";
        Restart = "on-failure";
        RestartSec = 5;
      };
    };
  };
}
