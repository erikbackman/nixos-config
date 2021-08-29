{ config, lib, pkgs, ... }:

with lib;
let cfg = config.programs.ebn.kitty;
in {
  options.programs.ebn.kitty = {
    enable = mkEnableOption "Enable Kitty";

    config = mkIf cfg.enable {
      fonts.fonts = with pkgs; [
        jetbrains-mono
      ];
      environment.systemPackages = [
        (pkgs.writeScriptBin "kitty"
        ''${pkgs.kitty}/bin/kitty -c ${./config/kitty.conf}'')
      ];
    };
  };
}
