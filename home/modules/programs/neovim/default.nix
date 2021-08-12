{ config, lib, pkgs, ... }:
{
  programs.neovim = {
    enable = true;
    package = pkgs.neovim;
    plugins = with pkgs.vimPlugins; [
      vim-startify
      vim-nix
      haskell-vim
      completion-nvim
      pkgs.ebn.spaceduck
      pkgs.ebn.aurora-vim
      popup-nvim
      plenary-nvim
      telescope-nvim
      nvim-lspconfig
      lualine-nvim
    ];
    extraConfig = builtins.readFile ./config/init.vim;
  };
  # I choose not to have Nix manage this directory for now until my config it somewhat done
  # home.file.".config/nvim/lua".source = ./config/lua;
}
