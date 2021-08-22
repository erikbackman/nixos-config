{ config, lib, pkgs, ... }:

with lib;
let cfg = config.programs.ebn.kitty;
in {
  options.programs.ebn.kitty = {
    enable = mkEnableOption "Enable Kitty";
    extraSettings = mkOption {
      type = types.attrs;
      default = {};
    };
    settings = mkOption {
      type = types.attrs;
      default = {
        dynamic_background_opacity = false;
        window_padding_width = 10;
        default_pointer_shape = "beam";
        cursor_shape = "underline";
        tab_bar_style = "powerline";
        tab_powerline_style = "round";
        active_tab_foreground = "#000";
        active_tab_background = "#eee";
        inactive_tab_foreground = "#444";
        inactive_tab_background = "#999";
      };
    };
  };

  config = mkIf cfg.enable {
    programs.kitty = {
      enable = true;
      font = {
        package = pkgs.jetbrains-mono;
        name = "JetBrains mono";
        size = 11;
      };
      settings = cfg.settings // cfg.extraSettings;
    };
  };
}
