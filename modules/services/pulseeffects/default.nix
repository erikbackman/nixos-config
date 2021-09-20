{ config, lib, pkgs, ... }:

let
  cfg = config.services.ebn.pulseeffects;
in with lib; {
  options.services.ebn.pulseeffects = {
    enable = mkEnableOption "Enable PulseEffects";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.pulseeffects-legacy pkgs.at-spi2-core ];

    systemd.user.services.pulseeffects = {
      description = "Polybar daemon"; 
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
