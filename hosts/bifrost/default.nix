{ pkgs, lib, config, ... }: 
{
  networking.hostName = "bifrost";
  networking.wireless = {
    enable = true;
    userControlled.enable = true;
  };

  ebn.themes = {
    enable = true;
    palette = "aurora";
  };

  nixpkgs.config.allowUnfree = true;
  programs.ebn.nvim.enable = true;
  programs.ebn.kitty.enable = true;
  programs.ebn.rofi.enable = true;
  programs.ebn.bash.enable = true;
  programs.ebn.bash.starship.enable = true;
  programs.ebn.emacs.enable = true;
  programs.ebn.emacs.package = pkgs.emacsGcc;

  services.redshift = {
    enable = true;
    inherit (config location);
  };

  # Xmonad
  windowManager.ebn.xmonad.enable = true;

  services.ebn.polybar.enable = true;
  services.fstrim.enable = true;

  services.ebn.syncthing.enable = true;
  services.xserver.layout = "se";
  services.dbus = {
    enable = true;
    packages = [ pkgs.gnome3.dconf ];
  };

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
    texlive.combined.scheme-full
    libreoffice
    firefox
    playerctl
    gsettings-desktop-schemas
    gnumake
    neofetch
    feh
    rofi-pass
    cowsay
    ncspot
    pavucontrol
    anki
    gnome.gucharmap
    wpa_supplicant
    man-pages
  ];
}
