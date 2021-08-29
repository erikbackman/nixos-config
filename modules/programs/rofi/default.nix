{ config, lib, pkgs, ... }:

with lib;
let 
  cfg = config.programs.ebn.rofi;
  packaged = pkgs.rofi;
in {
  options.programs.ebn.rofi = {
    enable = mkEnableOption "Enable Rofi";
  };

  config = mkIf cfg.enable {
    environment.etc."rofi/rofi.rasi".source = ./config/rofi.rasi;
    environment.systemPackages = with pkgs; [
      paper-icon-theme
      font-awesome
      roboto-mono
      (pkgs.writeScriptBin "rofi" ''
        ${pkgs.bash}/bin/bash exec rofi --config /etc/rofi/rofi.rasi "$@"
      '';
    ];
  };
}
