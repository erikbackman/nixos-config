{ config, options, lib, ... }:

let 
  mkOpt = type: default: lib.mkOption { inherit type default; };
  palettes = import ./palettes.nix;
  converters = import ./converters.nix;
  cfg = config.ebn.themes;

in
{
  options = with lib; with types; {
    ebn.themes.enable = mkEnableOption "Enable themes";
    ebn.themes.palette = mkOpt str "aurora";
  };

  config =
    let
      palette =
        if lib.hasAttr cfg.palette palettes then
          palettes.${cfg.palette}
        else palettes.aurora;
    in
      with converters; lib.mkIf cfg.enable {
        programs.ebn.kitty.extraConfig = paletteToKitty palette;
        services.ebn.polybar.extraConfig = paletteToPolybar palette;
      };
}
