{ config, lib, pkgs, ... }:

with lib;
let cfg = config.programs.ebn.bash;
in {
  options.programs.ebn.bash = {
    enable = mkEnableOption "Enable Bash";
    starship = { enable = mkEnableOption "Enable Starship Prompt"; };
    direnv = { enable = mkEnableOption "Enable Direnv"; };
  };

  config = mkIf cfg.enable {

    fonts.fonts = with pkgs; [ 
      (nerdfonts.override { fonts = [ "JetBrainsMono" ]; })
    ];

    environment.systemPackages = 
      (optionals cfg.direnv.enable [pkgs.direnv pkgs.nix-direnv]);

    nix.extraOptions = optionalString cfg.direnv.enable ''
      keep-outputs = true
      keep-derivations = true
    '';

    environment.pathsToLink = optionals cfg.direnv.enable [
      "/share/nix-direnv"
    ];

    nixpkgs.overlays = optionals cfg.direnv.enable [
      (self: super: { nix-direnv = super.nix-direnv.override { enableFlakes = false; }; } )
    ];

    environment.etc."starship/starship.toml".source = ./config/starship.toml;
    environment.sessionVariables = {
      "STARSHIP_CONFIG" = "/etc/starship/starship.toml";
    };

    environment.etc.bashrc.text = ''
      export GITUSER="$(git config -f $HOME/.config/git/config --get user.name)"
      export DOTFILES="$HOME/repos/github.com/$GITUSER/nixos-config"
      export GHREPOS="$HOME/repos/github.com/$GITUSER/"
      export STARSHIP_CONFIG=/etc/starship/starship.toml

      alias grep="grep --colour=auto"
      alias egrep="egrep --colour=auto"
      alias fgrep="fgrep --colour=auto"
      alias du="du -h -a --total"
      alias la="ls -al"
      alias nb="nix build"
      alias v="nvim"
      alias vim="nvim"
      alias vi="nvim"
      alias gs="git status"
      alias ga="git add"
      alias gc="git commit"
      alias gp="git push"

      p() {
        cd $(find $GHREPOS -maxdepth 1 ! -path $GHREPOS -type d | fzf)
      }
      eval "$(${pkgs.starship}/bin/starship init bash)"
      eval "$(${pkgs.direnv}/bin/direnv hook bash)"
    '' ;
    #+ (optionalString cfg.starship.enable ''eval "$(${pkgs.starship}/bin/starship init bash)"'');
  };
 
} 

