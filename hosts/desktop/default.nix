{ pkgs, lib, config, ... }: {

  networking.hostName = "yggdrasil";

  nixpkgs.config.allowUnfree = true;

  programs.steam.enable = true;
  programs.ebn.nvim.enable = true;
  programs.ebn.kitty.enable = true;
  programs.ebn.rofi.enable = true;
  programs.ebn.bash.enable = true;
  programs.ebn.bash.starship.enable = true;
  programs.ebn.emacs.enable = true;
  programs.ebn.emacs.version = "emacsPgtkGcc";

  # Xmonad
  windowManager.ebn.xmonad.enable = false;
  services.ebn.polybar.enable = false;
  #services.xserver.displayManager.sessionCommands = ''
  #  xrandr --output DP-0 --mode 3440x1440 --rate 99.98
  #'';

  # Gnome
  desktopEnvironment.ebn.gnome.enable = true;
  desktopEnvironment.ebn.gnome.withPopShell = true;


  services.ebn.syncthing.enable = true;
  services.ebn.pulseeffects.enable = true;
  services.xserver.videoDrivers = [ "nvidia" ];
  services.xserver.layout = "se";
  services.fstrim.enable = true;

  fonts.fonts = with pkgs; [
    etBook
    google-fonts
    hack-font
    iosevka jetbrains-mono
    oldstandard
    siji
  ];

  environment.systemPackages = with pkgs; [
    zathura
    maxima
    wxmaxima
    texlive.combined.scheme-full
    libreoffice
    chromium
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
    taskwarrior
    dstask
    anki
    sage
  ] 
  ++ 
  lib.lists.optional (! config.desktopEnvironment.ebn.gnome.enable) claws-mail;

  system.stateVersion = "20.03";
}
