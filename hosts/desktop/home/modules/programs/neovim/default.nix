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
      plugins = with pkgs.vimPlugins; [
        completion-nvim
        fzf-vim
        haskell-vim
        lualine-nvim
        nnn-vim
        nvim-lspconfig
        vim-nix
        vim-sneak
        vim-startify
      ];
      extraConfig = builtins.readFile ./config/init.vim;
    };

    home.packages = with pkgs; [
      fzf
    ];

    home.activation.linkMutableLuaConfig = lib.hm.dag.entryAfter [ "writeBoundry" ] ''
      ln -sf ${./config/lua} $HOME/.config/nvim/lua
    '';
  };
}
