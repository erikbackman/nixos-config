{ lib, config, pkgs, inputs, hm-modules-path, ... }: {
  programs.home-manager.enable = true;

  lib.hm.users.ebn = { pkgs, ... }: {
    nixpkgs.config.allowUnfree = true;
  };

  # This will import every default.nix found in ./modules
  imports = with (import ./lib.nix lib).modules;
    listModulesRec hm-modules-path;

  #############################################################################
  #                                  Programs                                 #
  #############################################################################
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
      extraConfig.core = {
        editor = "nvim";
      };
    };

    ncmpcpp = {
      enable = true;
      package = pkgs.ncmpcpp.override {
        outputsSupport = true;
        visualizerSupport = true;
        clockSupport = true;
      };
      settings = {
        visualizer_in_stereo = "no";
        visualizer_fifo_path = "~/.local/share/mpd/fifo";
        visualizer_output_name = "my_fifo";
        visualizer_sync_interval = "10";
        visualizer_type = "wave_filled";
        visualizer_color = "gray, magenta, magenta, magenta, magenta";
      };
    };

    ebn.kitty = {
      enable = true;
      extraSettings = import ./config/kittyThemes/aurora.nix;
    };

    ebn.emacs = {
      enable = true;
      version = "emacsGcc";
    };

    ebn.nvim.enable = true;

    ebn.rofi.enable = true;
  };

  #############################################################################
  #                                  XSession                                 #
  #############################################################################
  xsession.windowManager.ebn.xmonad.enable = true;

  #############################################################################
  #                                  Services                                 #
  #############################################################################
  services.ebn.polybar.enable = true;

  services.ebn.picom = {
    enable = true;
    opacityEnabled = false;
  };

  services.pulseeffects = {
    enable = true;
    package = pkgs.pulseeffects-legacy;
    preset = "ebn-preset";
  };

  services.mpd = {
    enable = true;
    network.startWhenNeeded = true;
    extraConfig = ''
        audio_output {
          type "pulse"
          name "pulse audio"
        }
        audio_output {
          type "fifo"
          name "my_fifo"
          path "~/.local/share/mpd/fifo"
          format "44100:16:2"
        }
    '';
  };

  #############################################################################
  #                           Programming Languages                           #
  #############################################################################
  ebn.dev.cc.enable = true;

  #############################################################################
  #                               User Packages                               #
  #############################################################################
  home.packages = with pkgs; [
    zathura
    spotify
    maxima
    wxmaxima
    texlive.combined.scheme-full
    libreoffice
    chromium
    playerctl
    gsettings-desktop-schemas
    gnumake
    obs-studio
    claws-mail-gtk3
    neofetch
    dracula-theme
    mpc_cli
    xmobar
    mpv
    streamlink
    feh
    discord
    gnome.gucharmap
    gimp
    qutebrowser
    rofi-pass
    kdenlive
    cowsay
    ncspot
    pavucontrol
    taskwarrior
    dstask
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
