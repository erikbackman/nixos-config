# NOTE: I'm slowly migrating away from home-manager
# This file will continue to shrink until it's no more :p
{ lib, config, pkgs, inputs, hm-modules-path, ... }: {
  programs.home-manager.enable = true;

  lib.hm.users.ebn = { pkgs, ... }: {
    nixpkgs.config.allowUnfree = true;
  };

  # This will import every default.nix found in hm-modules-path
  # (see flake.nix)
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

    ebn.emacs = {
      enable = true;
      version = "emacsGcc";
    };

  };

  services.ebn.picom = {
    enable = true;
    opacityEnabled = false;
  };

  #############################################################################
  #                           Programming Languages                           #
  #############################################################################
  ebn.dev.cc.enable = true;

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
