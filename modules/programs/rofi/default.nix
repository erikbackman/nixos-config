{ config, lib, pkgs, ... }:

with lib;
let 
  cfg = config.programs.ebn.rofi;
  wrapped = pkgs.writeShellScriptBin "rofi" ''
    exec ${pkgs.rofi}/bin/rofi -config /etc/rofi/rofi.rasi $@
  '';
  package = pkgs.symlinkJoin {
    name = "rofi";
    paths = [
      wrapped
      pkgs.rofi
    ];
  };
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
      package 
    ];
  };
}
