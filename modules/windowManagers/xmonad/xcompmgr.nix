{ pkgs, config, lib, ... }:

let
  cfg = config.services.ebn.xcompmgr;
in with lib; {
  options.services.ebn.xcompmgr = {
    enable = mkEnableOption "Enable xcompmgr Compositor";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.xcompmgr ];

    systemd.user.services.xcompmgr = {
      description = "XCompmgr Compositor"; 
      wantedBy = [ "graphical-session.target" ];
      after = [ "graphical-session-pre.target" ];
      partOf = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.xcompmgr}/bin/xcompmgr -c -C -t-5 -l-5 -r4.2 -o.55";
        ExecStop = "pkill xcompmgr";
        Restart = "on-failure";
        RestartSec = 5;
      };
    };
  };
}
