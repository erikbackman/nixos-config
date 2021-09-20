{ pkgs, lib, ... }: {

  networking.hostName = "nixos";

  nixpkgs.config.allowUnfree = true;

  programs.steam.enable = true;
  programs.ebn.nvim.enable = true;
  programs.ebn.kitty.enable = true;
  programs.ebn.rofi.enable = true;
  programs.ebn.bash.enable = true;
  programs.ebn.bash.starship.enable = true;
  programs.ebn.emacs.enable = true;
  programs.ebn.emacs.version = "emacsGcc";

  #programs.gnupg = {
  #  agent.enable = true;
  #  agent.enableSSHSupport = true;
  #};

  windowManager.ebn.xmonad.enable = true;

  services.ebn.polybar.enable = true;
  services.ebn.pulseeffects.enable = true;
  services.xserver.videoDrivers = [ "nvidia" ];
  services.xserver.layout = "se";
  services.xserver.displayManager.sessionCommands = ''
    xrandr --output DP-0 --mode 3440x1440 --rate 99.98
  '';
  services.dbus = {
    enable = true;
    packages = [ pkgs.gnome3.dconf ];
  };

  fonts.fonts = with pkgs; [
    etBook
    google-fonts
    hack-font
    iosevka
    jetbrains-mono
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
    claws-mail-gtk3
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
  ];
}
