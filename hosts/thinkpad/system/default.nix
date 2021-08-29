{ pkgs, lib, ... }: {

  networking.hostName = "bifrost";

  nixpkgs.config.allowUnfree = true;
  programs.steam.enable = true;
  programs.ebn.nvim.enable = true;
  programs.ebn.kitty.enable = true;
  windowManager.ebn.xmonad.enable = true;

  services.xserver.videoDrivers = [ "nvidia" ];
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
}
