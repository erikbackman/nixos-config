{ config, lib, pkgs, inputs, ... }:

with lib;
let
  cfg = config.programs.ebn.emacs;
  version = config.programs.ebn.emacs.version;
  emacsPackage =
    (pkgs.emacsWithPackagesFromUsePackage {
      config = ./config/ebn-core.el;
      alwaysEnsure = false;
      package = pkgs."${version}";
      extraEmacsPackages = epkgs: with epkgs; [ vterm ];
    });

in {
  options = {
    programs.ebn.emacs = {
      enable = mkEnableOption "Enable Emacs";
      version = mkOption {
        type = types.str;
        default = "emacsGit";
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      emacsPackage

      ## Doom dependencies
      git
      (ripgrep.override { withPCRE2 = true; })
      gnutls # for TLS connectivity
      emacs-all-the-icons-fonts
      coreutils
      binutils

      ## Optional dependencies
      fd # faster projectile indexing
      imagemagick # for image-dired
      zstd # for undo-fu-session/undo-tree compression

      ## Fonts
      jetbrains-mono

      ## Module dependencies
      # :checkers spell
      (aspellWithDicts (ds: with ds; [ en en-computers en-science ]))
      # :checkers grammar
      languagetool
      # :tools editorconfig
      editorconfig-core-c # per-project style config
      # :tools lookup & :lang org +roam
      sqlite
      # :lang cc
      ccls
      # :lang nix
      nixfmt
      # :term vterm
      libtool
      gnumake
      libvterm
      # :app everywhere
      xorg.xwininfo
      xclip
      # :org ob-jupyter
      jupyter
    ];
    services.emacs = {
      enable = true;
      package = emacsPackage;
    };

    xresources.properties = {
      "Emacs.menuBar" = false;
      "Emacs.toolBar" = false;
      "Emacs.verticalScrollBars" = false;
      "Emacs.Font" = "JetBrains Mono:size=14";
    };
  };
}
