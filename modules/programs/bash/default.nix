{ config, lib, pkgs, ... }:

with lib;
let 
  cfg = config.programs.ebn.bash;
in {
  options.programs.ebn.bash = {
    enable = mkEnableOption "Enable Bash";
    starship = { enable = mkEnableOption "Enable Starship Prompt"; };
  };

  config = mkIf cfg.enable {

    fonts.fonts = 
      (optionals cfg.starship.enable [ (pkgs.nerdfonts.override { fonts = [ "JetBrainsMono" ]; }) ]);

    environment.systemPackages = [pkgs.direnv pkgs.nix-direnv pkgs.starship];

    nix.extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';

    environment.pathsToLink = [
      "/share/nix-direnv"
    ];

    nixpkgs.overlays = [
      (self: super: { nix-direnv = super.nix-direnv.override { enableFlakes = false; }; } )
    ];

    environment.etc.bashrc.text = 
      if !cfg.starship.enable then
        builtins.readFile ./config/bashrc + ''
          eval "$(${pkgs.direnv}/bin/direnv hook bash)"
        ''
        else ''
          export GITUSER="$(git config -f $HOME/.config/git/config --get user.name)"
          export DOTFILES="$HOME/repos/github.com/$GITUSER/nixos-config"
          export GHREPOS="$HOME/repos/github.com/$GITUSER/"
          export STARSHIP_CONFIG=${pkgs.writeText "starship.toml" (builtins.readFile ./config/starship.toml)};

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

          eval "$(${pkgs.direnv}/bin/direnv hook bash)"
          eval "$(${pkgs.starship}/bin/starship init bash)"
          source ${./config/completions}
        '';
  };
 
} 

