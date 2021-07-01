{ lib, config, pkgs, ... }:

{
  services.dunst = {
    enable = true;
    iconTheme = {
      name = "BeautyLine";
      package = pkgs.beauty-line-icon-theme;
      size = "16x16";
    };
    settings = {
      global = {
        alignment = "left";
        bounce_freq = 0;
        corner_radius = 2;
        dmenu = "rofi -dmenu -p dunst";
        follow = "none";
        font = "Roboto Mono 11";
        format = ''
          <b>%s</b>
          %b'';
        frame_color = "#1a1c25";
        frame_width = 1;
        geometry = "440x15-26+26";
        history_length = 20;
        horizontal_padding = 16;
        icon_position = "right";
        idle_threshold = 120;
        ignore_newline = "no";
        indicate_hidden = "yes";
        line_height = 0;
        markup = "full";
        max_icon_size = 64;
        monitor = 0;
        padding = 20;
        separator_color = "auto";
        separator_height = 4;
        show_age_threshold = 60;
        show_indicators = "yes";
        shrink = "no";
        sort = "yes";
        startup_notification = false;
        sticky_history = "yes";
        transparency = 5;
        word_wrap = "yes";
      };
      urgency_low = {
          background = "#1E2029";
          foreground = "#bbc2cf";
          timeout = 8;
      };
      urgency_normal = {
          background = "#2a2d39";
          foreground = "#bbc2cf";
          timeout = 14;
      };
      urgency_critical = {
          background = "#cc6666";
          foreground = "#1E2029";
          timeout = 0;
      };
    };
  };
}
