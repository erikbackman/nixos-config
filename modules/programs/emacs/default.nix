{ config, lib, pkgs, inputs, ... }:

with lib;
let
  cfg = config.programs.ebn.emacs;
  version = config.programs.ebn.emacs.version;
  emacsPackage =
    let 
      configTxt = (builtins.readFile ./config/ebn-init.el);

      init = pkgs.runCommand "default.el" {} ''
        mkdir -p $out/share/emacs/site-lisp
        cp ${pkgs.writeText "default.el" configTxt} $out/share/emacs/site-lisp/default.el
      '';
      
      package = cfg.package.override {
        siteStart = pkgs.writeText "site-start.el" (
          builtins.readFile ./config/site-start.el
        );
      };

    in
      (pkgs.emacsWithPackagesFromUsePackage {
        config = configTxt;
        alwaysEnsure = true;
        package = package; 
        extraEmacsPackages = epkgs: with epkgs; [ vterm init agda2-mode use-package pkgs.ebn.ebn-core-el ];
        override = epkgs: epkgs // {
          kaolin-themes = pkgs.ebn.kaolin-themes;
        }; 
      });
in {

  options = {
    programs.ebn.emacs = {
      enable = mkEnableOption "Enable Emacs";
      package = mkOption {
        type = lib.types.package;
        default = pkgs.emacsGit;
      };
    };
  };

  config = mkIf cfg.enable {

    fonts.fonts = with pkgs; [
      ebn.iosevka-custom
      jetbrains-mono
      cm_unicode
    ];
    
    environment.systemPackages = with pkgs; [
      emacsPackage

      ## Dependencies
      git
      (ripgrep.override { withPCRE2 = true; })
      gnutls # for TLS connectivity
      emacs-all-the-icons-fonts
      coreutils
      binutils

      fd # faster projectile indexing
      imagemagick # for image-dired
      zstd # for undo-fu-session/undo-tree compression

      ## Package dependencies
      # :checkers spell
      (aspellWithDicts (ds: with ds; [ en en-computers en-science ]))

      # :checkers grammar
      languagetool

      # editorconfig
      editorconfig-core-c # per-project style config

      # org
      gnuplot
      dot2tex
      graphviz

      # org-roam
      sqlite

      # cc
      ccls

      # nix
      nixfmt

      # vterm
      libtool
      gnumake
      libvterm

      # emacs everywhere
      xorg.xwininfo
      xclip

      # ob-jupyter
      (python38.withPackages(ps: [ ps.jupyter ps.python-lsp-server ]))

      # agda
      haskellPackages.Agda

      # cc
      gcc
      ccls
      global

      # python
      yapf
    ];
  };
}
