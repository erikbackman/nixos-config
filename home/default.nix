{ lib, config, pkgs, inputs, myLib, ... }: {
  programs.home-manager.enable = true;

  lib.hm.users.ebn = { pkgs, ... }: {
    nixpkgs.config.allowUnfree = true;
  };

  imports = with (import ./lib.nix lib).modules;
    listModulesRec ./modules;

  programs = {

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    bash = {
      enable = true;
      bashrcExtra = builtins.readFile ./config/bashrc;
    };

    git = {
      enable = true;
      userName = "erikbackman";
      userEmail = "erikbackman@users.noreply.github.com";

      extraConfig = { core = { editor = "emacs"; }; };
    };

    kitty = {
      enable = true;
      font = {
        package = pkgs.jetbrains-mono;
        name = "JetBrains mono";
        size = 11;
      };
      settings = {
        background           = "#0C0F12";
        foreground           = "#e6e8ee";
        cursor               = "#f6f6ec";
        selection_background = "#2e353d";
        color0               = "#22252b";
        color8               = "#22252b";
        color1               = "#b53f36";
        color9               = "#b53f36";
        color2               = "#5ab977";
        color10              = "#5ab977";
        color3               = "#ddb566";
        color11              = "#ddb566";
        color4               = "#6a7b92";
        color12              = "#6a7b92";
        color5               = "#9d81ba";
        color13              = "#9d81ba";
        color6               = "#3f93a8";
        color14              = "#3f93a8";
        color7               = "#e6e8ee";
        color15              = "#ebedf2";
        selection_foreground = "#1b1d22";

        dynamic_background_opacity = true;
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

  ebn = {
    # C/C++
    dev.cc.enable = true;

    emacs = {
      enable = true;
      version = "emacsGcc";
    };

    windowManager.xmonad.enable = true;

    programs.rofi.enable = true;
  };

  fonts.fontconfig.enable = true;

  nixpkgs.config.allowUnfree = true;

  home.packages = with pkgs; [
    zathura
    spotify
    maxima
    wxmaxima
    texlive.combined.scheme-full
    libreoffice-qt
    arc-theme
    qogir-theme
    chromium
    nyxt
    lyx
    nitrogen
    playerctl
    gsettings-desktop-schemas
    playonlinux
    gnumake
    polybar
  ];

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.03";
}
