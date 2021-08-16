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
    sumneko
    fzf
  ];

  home.file.".cache/nvim/lspconfig/sumneko_lua/lua-language-server".source = sumneko_bin;
}
