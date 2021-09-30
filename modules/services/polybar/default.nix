{ config, lib, pkgs, ... }:

let
  cfg = config.services.ebn.polybar;
  package = pkgs.polybar.override {
    alsaSupport = true;
    pulseSupport = true;
    mpdSupport = true;
  };
  polybarCfg = ''
    ${builtins.readFile ./config/polybarrc}
    [module/xmonad]
    type = custom/script
    exec = ${pkgs.xmonad-log}/bin/xmonad-log
    tail = true
  '';
in with lib; {
  options.services.ebn.polybar = {
    enable = mkEnableOption "Enable polybar";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ package ];
    fonts.fonts = with pkgs; [ font-awesome siji jetbrains-mono ];

    services.dbus = {
      enable = true;
      packages = [ pkgs.gnome3.dconf ];
    };

    environment.etc."polybar/polybarrc".text = polybarCfg;

    systemd.user.services.polybar = {
      description = "Polybar status bar";
      partOf = [ "tray.target" ];
      serviceConfig = {
        Type = "forking";
        Environment = "PATH=${package}/bin:/run/wrappers/bin";
        ExecStart =
          let scriptPkg = 
            pkgs.writeShellScriptBin "polybar-start" "polybar main -c /etc/polybar/polybarrc &";
          in "${scriptPkg}/bin/polybar-start";
        Restart = "on-failure";
      };
    };
  };
}
