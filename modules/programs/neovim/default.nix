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
          ];
        };
      };
    };

    environment.systemPackages = with pkgs; [
      fzf
    ];

    #home.file.".config/nvim/lua".source = ./config/lua;
    #home.activation.copyLuaConfig = lib.hm.dag.entryAfter [ "writeBoundry" ] ''
    #  if [[ -d $HOME/.config/nvim/lua ]]; then
    #    rm -rf $HOME/.config/nvim/lua
    #  fi

    #  cp -rf ${./config/lua} $HOME/.config/nvim/lua
    #'';
  };
}
