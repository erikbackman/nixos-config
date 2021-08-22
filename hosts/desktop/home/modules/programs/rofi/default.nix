{ config, lib, pkgs, ... }:

with lib;
let cfg = config.programs.ebn.rofi;
in {
  options.programs.ebn.rofi = {
    enable = mkEnableOption "Enable Rofi";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      paper-icon-theme
      font-awesome
      roboto-mono
      rofi
    ];
    programs.rofi = {
      enable = true;
      theme = ./config/rofi.rasi;
    };
  };
}
