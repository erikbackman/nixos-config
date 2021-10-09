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
      dataDir = "/home/ebn";
      openDefaultPorts = true;
      configDir = "/home/ebn/.config/syncthing";
      user = "ebn";
      group = "users";
      guiAddress = "0.0.0.0:8384";
      overrideDevices = true;
      overrideFolders = true;

      devices = {
        "yggdrasil" = { id = "5RLYA4M-B6TJNPG-Z57KRSR-BEGEIZT-4WFPGUX-KQ72XE6-TMTWHZL-2K6PPAH"; };
        "bifrost" = { id = "7LCKHMQ-7EJNDGY-CUQK5M3-X5MGEV2-F7FIKL5-VQ3GZUL-MWL4CVN-3VVW2QX"; };
      };
      folders = {
        "roam" = {
          path = "home/ebn/org-roam";
          devices = [ "yggdrasil" "bifrost" ];
      };
      "lab" = {
        path = "home/ebn/Documents/lab";
        devices = [ "yggdrasil" "bifrost" ];
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
