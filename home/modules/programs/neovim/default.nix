{ config, lib, pkgs, ... }:
let 
  sumneko       = pkgs.sumneko-lua-language-server;
  sumneko_bin   = "${sumneko}/bin/lua-language-server";
in
{
  programs.neovim = {
    enable = true;
    package = pkgs.neovim;
    plugins = with pkgs.vimPlugins; [
      nlua-nvim
      vim-startify
      vim-nix
      haskell-vim
      completion-nvim
      pkgs.ebn.spaceduck
      popup-nvim
      plenary-nvim
      nvim-lspconfig
      lualine-nvim
      nnn-vim
      fzf-vim
    ];
    extraConfig = builtins.readFile ./config/init.vim;
  };

  home.packages = with pkgs; [
    sumneko
    fzf
  ];

  home.file.".cache/nvim/lspconfig/sumneko_lua/lua-language-server".source = sumneko_bin;
}
