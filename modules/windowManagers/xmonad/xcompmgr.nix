{ pkgs, config, lib, ... }:

let
  cfg = config.services.ebn.xcompmgr;
in with lib; {
  options.services.ebn.xcompmgr = {
    enable = mkEnableOption "Enable xcompmgr Compositor";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ xcompmgr ];

    systemd.user.services.xcompmgr = {
      description = "XCompmgr Compositor"; 
      wantedBy = [ "graphical-session.target" ];
      after = [ "graphical-session-pre.target" ];
      partOf = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.xcompmgr}/bin/xcompmgr";
        ExecStop = "pkill xcompmgr";
        Restart = "on-failure";
        RestartSec = 5;
      };
    };
  };
}
