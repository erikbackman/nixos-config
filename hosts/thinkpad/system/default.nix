{ pkgs, lib, ... }: {

  networking.hostName = "bifrost";

  nixpkgs.config.allowUnfree = true;
  programs.steam.enable = true;
  programs.ebn.nvim.enable = true;
  windowManager.ebn.xmonad.enable = true;

  fonts.fonts = with pkgs; [
    etBook
    google-fonts
    hack-font
    iosevka
    jetbrains-mono
    oldstandard
    siji
  ];

  services = {
    dbus = {
      enable = true;
      packages = [ pkgs.gnome3.dconf ];
    };
  };
}
