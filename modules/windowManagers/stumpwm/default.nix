{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.windowManager.ebn.stumpwm;
  ebn-stumpwm = pkgs.ebn.ebn-stumpwm;
in {
  options.windowManager.ebn.stumpwm = {
    enable = mkEnableOption "ebn stumpwm";
  };

  config = mkIf cfg.enable {
    services.xserver.windowManager.session = singleton {
      name = "ebn-stumpwm";
      start = ''
        ${ebn-stumpwm}/bin/ebn-stumpwm.sh &
        waitPID=$!
      '';
    };
    environment.systemPackages = [ebn-stumpwm];
  };
}
