{ config, lib, pkgs, ... }:

with lib;
let cfg = config.desktopEnvironment.ebn.gnome;
    usesNvidia = lib.lists.elem "nvidia" config.services.xserver.videoDrivers;
in {
  options.desktopEnvironment.ebn.gnome = {
    enable = mkEnableOption "Enable Gnome";
    withPopShell = mkEnableOption "Enable Pop-Shell";
  };

  config = mkIf cfg.enable {
    fonts.fonts = with pkgs; [
      jetbrains-mono
    ];

    hardware.nvidia.modesetting.enable = usesNvidia;
    hardware.opengl.enable = true;

    services.xserver = {
      enable = true;
      layout = "se";
      displayManager.gdm = {
        enable = true;
        nvidiaWayland = usesNvidia;
        autoSuspend = false;
      };
      desktopManager.gnome.enable = true;
    };

    environment.etc."/egl/egl_external_platform.d/nvidia_wayland.json".text = ''
      "file_format_version" : "1.0.0",
      "ICD" : {
          "library_path" : "/run/opengl-driver/lib/libnvidia-egl-wayland.so"
      }
    '';

    services.dbus.packages = [ pkgs.gnome3.dconf ];
    services.udev.packages = [ pkgs.gnome3.gnome-settings-daemon ];

    programs.xwayland.enable = true;
    programs.xwayland.package = pkgs.xwayland;
    environment.gnome.excludePackages = [ 
      pkgs.gnome.cheese pkgs.gnome-photos pkgs.gnome.gnome-music 
      #pkgs.gnome.gnome-terminal 
      pkgs.gnome.gedit pkgs.epiphany pkgs.evince pkgs.gnome.gnome-characters pkgs.gnome.totem 
      pkgs.gnome.tali pkgs.gnome.iagno pkgs.gnome.hitori pkgs.gnome.atomix pkgs.gnome-tour 
    ];

    programs.dconf.enable = true;

    environment.systemPackages = with pkgs; [ 
      gnome.adwaita-icon-theme 
      gnome.gnome-tweaks 
      xwayland

      materia-theme
      orchis-theme
      nordic
      papirus-icon-theme
      gnome-breeze
      maia-icon-theme

    ] ++ lib.optionals cfg.withPopShell [ ebn.pop-shell ];
  };
}
