{ config, lib, pkgs, ... }:

with lib;
let cfg = config.ebn.programs.rofi;
in {
  options.ebn.programs.rofi = {
    enable = mkEnableOption "Enable Rofi";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      paper-icon-theme
      rofi
    ];
    programs.rofi = {
      enable = true;
      theme = ./config/rofi.rasi;
    };
  };
}
