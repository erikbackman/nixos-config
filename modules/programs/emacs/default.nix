{ config, lib, pkgs, inputs, ... }:

with lib;
let
  cfg = config.programs.ebn.emacs;
  version = config.programs.ebn.emacs.version;
  emacsPackage =
    let 

    init = pkgs.runCommand "default.el" {} ''
      mkdir -p $out/share/emacs/site-lisp
      cp ${pkgs.writeText "default.el" (builtins.readFile ./config/ebn-core.el)} $out/share/emacs/site-lisp/default.el
    '';
    # ln -sf ${./early-init.el} $out/share/emacs/site-lisp/early-init.el
    in
    (pkgs.emacsWithPackagesFromUsePackage {
      config = ./config/ebn-core.el;
      alwaysEnsure = true;
      package = pkgs."${version}";
      extraEmacsPackages = epkgs: with epkgs; [ vterm init agda2-mode ];
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
    
    services.emacs.enable = true;
    services.emacs.package = emacsPackage;

    environment.systemPackages = with pkgs; [
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
      (python38.withPackages(ps: [ ps.jupyter]))

      gnuplot
      dot2tex
      graphviz
      haskellPackages.Agda
    ];

  };
}
