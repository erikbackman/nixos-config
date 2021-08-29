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
    environment.etc."kitty/kitty.conf".source = ./config/kitty.conf;
    environment.variables = {
      "KITTY_CONFIG_DIRECTORY" = "/etc/kitty";
    };
    environment.systemPackages = with pkgs; [
      kitty
      #(pkgs.writeScriptBin "kitty" ''
      #  #!${pkgs.runtimeShell}
      #  exec ${pkgs.kitty}/bin/kitty -c /etc/kitty/kitty.conf
      #''
      #)
    ];
  };
}
