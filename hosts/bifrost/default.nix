{ pkgs, lib, ... }: {

  networking.hostName = "bifrost";

  nixpkgs.config.allowUnfree = true;
  programs.ebn.nvim.enable = true;
  programs.ebn.kitty.enable = true;
  windowManager.ebn.xmonad.enable = true;

  services.xserver.layout = "se";
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
    texlive.combined.scheme-full
    libreoffice
    chromium
    playerctl
    gsettings-desktop-schemas
    gnumake
    neofetch
    feh
    rofi-pass
    cowsay
    ncspot
    pavucontrol
    taskwarrior
    dstask
    anki
  ];
}
