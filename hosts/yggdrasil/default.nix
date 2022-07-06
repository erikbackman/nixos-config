{ pkgs, lib, config, ... }: {

  networking.hostName = "yggdrasil";

  nixpkgs.config.allowUnfree = true;

  programs.steam.enable = true;
  programs.ebn.nvim.enable = true;
  programs.ebn.kitty.enable = false;
  programs.ebn.bash.enable = true;
  programs.ebn.bash.starship.enable = false;
  programs.ebn.emacs.enable = true;
  programs.ebn.emacs.package = pkgs.emacsNativeComp;

  services.urxvtd.enable = true;
  services.redshift = {
    enable = true;
    temperature.night = 3200;
    temperature.day = 3400;
    inherit (config location);
  };

  services.earlyoom.enable = true;
  services.clamav = {
    daemon.enable = true;
    updater.enable = true;
  };

  windowManager.ebn.xmonad.enable = true;
  windowManager.ebn.stumpwm.enable = false;
  services.ebn.tint2.enable = true;
  services.ebn.polybar.enable = false;
  services.xserver.enable = true;

  services.xserver.displayManager.sessionCommands = ''
    xrandr --output DP-0 --mode 3440x1440 --rate 99.98
    xset r rate 150 25
    ${pkgs.xorg.xrdb}/bin/xrdb -merge <${pkgs.writeText "Xresources" ''
      Emacs.menuBar: false
      Emacs.toolBar: false
      Emacs.verticalScrollBars: false
      Xft.dpi: 96
      URxvt*geometry: 240x84
      URxvt.font: xft:Iosevka:size=13
      URxvt.scrollBar: false
      URxvt.clipboard.autocopy: true
      URxvt.keysym.M-c: perl:clipboard:copy
      URxvt.keysym.M-v: perl:clipboard:paste

      ! special
      *.foreground:   #2e3338
      *.background:   #f7f7f7
      *.cursorColor:  #464646

      ! black
      *.color0:       #101010
      *.color8:       #2e3338

      ! red
      *.color1:       #7c7c7c
      *.color9:       #7c7c7c

      ! green
      *.color2:       #8e8e8e
      *.color10:      #8e8e8e

      ! yellow
      *.color3:       #a0a0a0
      *.color11:      #a0a0a0

      ! blue
      *.color4:       #525252
      *.color12:      #525252

      ! magenta
      *.color5:       #5c3e99
      *.color13:      #5c3e99

      ! cyan
      *.color6:       #868686
      *.color14:      #868686

      ! white
      *.color7:       #b9b9b9
      *.color15:      #f7f7f7
    ''}
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
    maxima
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
    qutebrowser
    tor
    tor-browser-bundle-bin
    trash-cli
    spotify
    guile_3_0
    texlab
    geogebra6
    gnome.gnome-boxes
    qemu_kvm
    lxappearance
    paper-icon-theme
    paper-gtk-theme
    lounge-gtk-theme
  ]
  ++
  lib.lists.optional (! config.desktopEnvironment.ebn.gnome.enable) claws-mail;

  system.stateVersion = "20.03";
}
