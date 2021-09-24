{ config, lib, pkgs, ... }:

with lib;
let cfg = config.desktopEnvironment.ebn.gnome;
in {
  options.desktopEnvironment.ebn.gnome = {
    enable = mkEnableOption "Enable Gnome";
  };

  config = mkIf cfg.enable {
    fonts.fonts = with pkgs; [
      jetbrains-mono
    ];
    services.xserver = {
      enable = true;
      layout = "se";
      displayManager.gdm.enable = true;
      displayManager.gdm.wayland = true;
      displayManager.gdm.nvidiaWayland = true;
      desktopManager.gnome.enable = true;

    };

    services.dbus.packages = [ pkgs.gnome3.dconf ];
    services.udev.packages = [ pkgs.gnome3.gnome-settings-daemon ];

    hardware.nvidia.modesetting.enable = true;

    programs.xwayland.enable = true;
    environment.gnome.excludePackages = [ 
      pkgs.gnome.cheese pkgs.gnome-photos pkgs.gnome.gnome-music 
      #pkgs.gnome.gnome-terminal 
      pkgs.gnome.gedit pkgs.epiphany pkgs.evince pkgs.gnome.gnome-characters pkgs.gnome.totem 
      pkgs.gnome.tali pkgs.gnome.iagno pkgs.gnome.hitori pkgs.gnome.atomix pkgs.gnome-tour 
    ];

    programs.dconf.enable = true;
    environment.systemPackages = with pkgs; [ gnome.adwaita-icon-theme gnome.gnome-tweaks ];
  };
}
