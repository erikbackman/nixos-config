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
        "yggrasil" = { id = "5RLYA4M-B6TJNPG-Z57KRSR-BEGEIZT-4WFPGUX-KQ72XE6-TMTWHZL-2K6PPAH"; };
      };
      folders = {
        "roam" = {
          path = "home/ebn/org-roam";
          devices = [ "yggdrasil" ];
      };
      "lab" = {
        path = "home/ebn/Documents/lab";
        devices = [ "yggdrasil" ];
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
