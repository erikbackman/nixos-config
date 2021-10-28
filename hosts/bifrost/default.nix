{ pkgs, lib, config, ... }: 
let theme = import ./theme.nix { inherit config lib; };
in {

  networking.hostName = "bifrost";

  nixpkgs.config.allowUnfree = true;
  programs.ebn.nvim.enable = true;
  programs.ebn.kitty.enable = true;
  programs.ebn.kitty.extraConfig = theme.kittyLight;
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
  services.ebn.polybar.extraConfig = theme.polybar;
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

  fonts.fontconfig = {
    enable = true;
    antialias = true;
    hinting = {
      enable = true;
    };
  };

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
