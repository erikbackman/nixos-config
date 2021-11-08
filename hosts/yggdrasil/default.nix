{ pkgs, lib, config, ... }: {
  networking.hostName = "yggdrasil";

  nixpkgs.config.allowUnfree = true;

  ebn.themes = {
    enable = true;
    palette = "aurora";
  };

  programs.steam.enable = true;
  programs.ebn.nvim.enable = true;
  programs.ebn.kitty.enable = true;
  programs.ebn.rofi.enable = true;
  programs.ebn.bash.enable = true;
  programs.ebn.bash.starship.enable = true;
  programs.ebn.emacs.enable = true;
  programs.ebn.emacs.package = pkgs.emacsGcc;

  services.redshift = {
    enable = true;
    temperature.night = 4000;
    inherit (config location);
  };

  windowManager.ebn.xmonad.enable = true;
  services.ebn.polybar.enable = true;
  services.xserver.displayManager.sessionCommands = ''
    xrandr --output DP-0 --mode 3440x1440 --rate 99.98
  '';

  services.ebn.syncthing.enable = true;
  services.ebn.pulseeffects.enable = true;
  services.xserver.videoDrivers = [ "nvidia" ];
  services.xserver.layout = "se";
  services.fstrim.enable = true;

  fonts.fonts = with pkgs; [
    etBook
    google-fonts
    hack-font
    (iosevka.override {
      privateBuildPlan = ''
        [buildPlans.iosevka-custom]
        family = "Iosevka Custom"
        spacing = "normal"
        serifs = "sans"
        no-cv-ss = true

        [buildPlans.iosevka-custom.variants]
        inherits = "ss08"

        [buildPlans.iosevka-custom.ligations]
        inherits = "haskell"
      '';
      set = "custom";
    })
    jetbrains-mono
    victor-mono
    oldstandard
    siji
  ];

  fonts.fontconfig = {
    enable = true;
    antialias = true;
    hinting = {
      enable = true;
    };
  };

  environment.systemPackages = with pkgs; [
    zathura
    maxima
    wxmaxima
    texlive.combined.scheme-full
    libreoffice
    firefox
    playerctl
    gsettings-desktop-schemas
    gnumake
    obs-studio
    neofetch
    mpc_cli
    mpv
    streamlink
    feh
    discord
    gnome.gucharmap
    gimp
    rofi-pass
    kdenlive
    cowsay
    ncspot
    pavucontrol
    anki
    sage
    geogebra6
    jekyll
    man-pages
  ] 
  ++ 
  lib.lists.optional (! config.desktopEnvironment.ebn.gnome.enable) claws-mail;

  system.stateVersion = "20.03";
}
