{ config, lib, pkgs, ... }:

with lib;
let cfg = config.programs.ebn.kitty;
in {
  options.programs.ebn.kitty = {
    enable = mkEnableOption "Enable Kitty";
  };

  config = mkIf cfg.enable {
    fonts.fonts = with pkgs; [
      jetbrains-mono
    ];
    environment.systemPackages = 
      let 
        config = pkgs.writeText "kitty config" (builtins.readFile ./config/kitty.conf);

        wrapped = pkgs.writeShellScriptBin "kitty" ''
          exec ${pkgs.kitty}/bin/kitty -c ${config}
        '';
      in
      [ wrapped ];
  };
}
