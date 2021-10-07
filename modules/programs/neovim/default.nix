{ config, lib, pkgs, inputs, ... }:
let
  cfg = config.programs.ebn.nvim;
in with lib; {
  options.programs.ebn.nvim = {
    enable = mkEnableOption "Enable ebn nvim";
  };

  config = mkIf cfg.enable {
    
    nixpkgs.overlays = [
      inputs.neovim-git.overlay
    ];

    programs.neovim = {
      enable = true;
      package = pkgs.neovim;
      configure = {
        customRC = builtins.readFile ./config/init.vim;
        packages.myVimPackage = with pkgs.vimPlugins; {
          start = [
            completion-nvim
            fennel-vim
            #aniseed
            fzf-vim
            haskell-vim
            lualine-nvim
            nnn-vim
            nvim-lspconfig
            vim-nix
            vim-sneak
            vim-startify
            pkgs.ebn.modus-theme-vim
            agda-vim
          ];
        };
      };
    };

    environment.systemPackages = with pkgs; [
      fzf
      #agda
      haskellPackages.Agda
    ];
  };
}
