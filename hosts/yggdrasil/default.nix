{ pkgs, lib, config, ... }: {
  networking.hostName = "yggdrasil";

  nixpkgs.config.allowUnfree = true;

  programs.steam.enable = true;
  programs.ebn.nvim.enable = true;
  programs.ebn.kitty.enable = true;
  programs.ebn.bash.enable = true;
  programs.ebn.bash.starship.enable = true;
  programs.ebn.emacs.enable = true;
  programs.ebn.emacs.package = pkgs.emacsNativeComp;

  services.redshift = {
    enable = true;
    temperature.night = 3200;
    temperature.day = 3200;
    inherit (config location);
  };

  services.earlyoom.enable = true;
  services.clamav = {
    daemon.enable = true;
    updater.enable = true;
  };

  windowManager.ebn.xmonad.enable = true;
  windowManager.ebn.stumpwm.enable = false;
  services.ebn.tint2.enable = false;
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
    fantasque-sans-mono
    monoid
  ];

  # environment.variables.WEBKIT_DISABLE_COMPOSITING_MODE = "1"; # nyxt
  
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
    feh
    discord
    gnome.gucharmap
    gimp
    rofi-pass
    kdenlive
    cowsay
    ncspot
    spotify-tui
    pavucontrol
    sage
    man-pages
    firefox
    chromium
    qutebrowser
    trash-cli
    spotify
    e17gtk
    guile_3_0
    texlab
    geogebra6
    gnome.gnome-boxes
    qemu_kvm
  ] 
  ++ 
  lib.lists.optional (! config.desktopEnvironment.ebn.gnome.enable) claws-mail;

  system.stateVersion = "20.03";
}
