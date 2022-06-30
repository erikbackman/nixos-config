{ config, lib, pkgs, ... }:

let
  cfg = config.services.ebn.tint2;

in with lib; {
  options.services.ebn.tint2 = {
    enable = mkEnableOption "Enable tint2 bar";
  };

  config = mkIf cfg.enable {
    fonts.fonts = with pkgs; [
    ];
    environment.systemPackages = [ pkgs.tint2 pkgs.pasystray pkgs.paper-icon-theme ];
    
    services.dbus = {
      enable = true;
      packages = [ pkgs.dconf ];
    };

    systemd.user.services.tint2 = {
      description = "Tint2 bar";
      partOf = [ "tray.target" ];
      serviceConfig = {
        Type = "forking";
        Environment = "PATH=${pkgs.tint2}/bin:/run/wrappers/bin:${pkgs.pasystray}/bin";
        ExecStart =
          let
            tint2rc = pkgs.writeText "tint2rc" (builtins.readFile ./config/tint2rc);
            scriptPkg =
              pkgs.writeShellScriptBin "tint2-start" "${pkgs.tint2}/bin/tint2 -c ${tint2rc} &";
          in "${scriptPkg}/bin/tint2-start";
        ExecStop = "pkill tint2";
        Restart = "on-failure";
      };
    };
  };
}
