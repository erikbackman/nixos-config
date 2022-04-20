{ pkgs, lib, config, ... }: {
  networking.hostName = "yggdrasil";

  nixpkgs.config.allowUnfree = true;

  programs.steam.enable = true;
  programs.ebn.nvim.enable = false;
  programs.ebn.kitty.enable = true;
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
  services.ebn.tint2.enable = true;
  services.ebn.polybar.enable = false;
  services.xserver.enable = true;
  services.xserver.displayManager.sessionCommands = ''
    xrandr --output DP-0 --mode 3440x1440 --rate 99.98
    xset r rate 150 25
  '';

  services.ebn.syncthing.enable = true;
  services.ebn.pulseeffects.enable = true;
  services.xserver.videoDrivers = [ "nvidia" ];
  services.xserver.layout = "se";
  services.fstrim.enable = true;
  
  fonts.fonts = with pkgs; [
    #etBook
    eb-garamond
    google-fonts
    hack-font
    iosevka
    #ebn.iosevka-custom
    jetbrains-mono
    #victor-mono
    oldstandard
    gyre-fonts
    #libre-baskerville
    siji
    cm_unicode
    bakoma_ttf
    lmmath
    #proggyfonts
    #tamsyn
    #julia-mono
    #recursive
  ];

  environment.systemPackages = with pkgs; [
    zathura
    #maxima
    #wxmaxima
    texlive.combined.scheme-full
    libreoffice
    playerctl
    gsettings-desktop-schemas
    gnumake
    obs-studio
    neofetch
    mpc_cli
    mpv
    #streamlink
    feh
    discord
    gnome.gucharmap
    gimp
    rofi-pass
    kdenlive
    cowsay
    ncspot
    pavucontrol
    #anki
    sage
    #geogebra6
    #jekyll
    man-pages
    #brave
    firefox
    trash-cli
    spotify
    e17gtk
  ] 
  ++ 
  lib.lists.optional (! config.desktopEnvironment.ebn.gnome.enable) claws-mail;

  system.stateVersion = "20.03";
}
