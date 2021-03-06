{ config, lib, pkgs, ... }:

with lib;
let cfg = config.services.ebn.syncthing;
in {
  options.services.ebn.syncthing = {
    enable = mkEnableOption "Enable Syncthing";
  };

  config = mkIf cfg.enable {
    services.syncthing = {
      enable = true;
      dataDir = "~";
      openDefaultPorts = true;
      configDir = "/home/ebn/.config/syncthing";
      user = "ebn";
      group = "users";
      #guiAddress = "127.0.0.1:8384";
      overrideDevices = true;
      overrideFolders = true;

      devices = {
        "yggdrasil" = { id = "H6MQKQH-YTA3RSJ-T2U7YER-4BROWZD-AT5OESQ-RCNFWZ5-TPHGQTN-HVWWEQ2"; };
        "bifrost" = { id = "7LCKHMQ-7EJNDGY-CUQK5M3-X5MGEV2-F7FIKL5-VQ3GZUL-MWL4CVN-3VVW2QX"; };
        "eriksamsung" = { id = "GHO7G3N-4LEGH32-VZPODIB-6AK2P7R-RSQ2CX4-YSMWP3O-56SA7TB-HV7LQA7"; };
        "erik-20fms1w30f" = { id = "DIULWPT-5JR4WBL-VIHXYI4-2P3KRBY-T4R2SKR-3FSD7LJ-VUBL7HA-VOSQQQ7"; };
      };
      folders = {
        #"roam" = {
        #  path = "home/ebn/org-roam";
        #  devices = [ "yggdrasil" "bifrost" ];
        #};
        "labb-grupp7" = {
          id = "annyx-pi9vs";
          path = "~/Documents/labb-grupp7";
          devices = [ "yggdrasil" "bifrost" "eriksamsung" "erik-20fms1w30f" ];
          versioning = {
            type = "staggered";
            params = {
              cleanInterval = "3600"; # 1 hour in seconds
              maxAge = "15552000"; # 180 days in seconds
            };
          };
        };
    };
  };
};
}
