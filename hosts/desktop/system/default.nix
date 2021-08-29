{ pkgs, lib, ... }: {
  #imports = [ 
  #  ./hardware-configuration.nix 
  #  ../../base
  #]; 
  #++ myModules; 

  networking = {
    hostName = "nixos";
  };

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

    #xserver = {
    #  enable = true;
    #  layout = "se";
    #  videoDrivers = [ "nvidia" ];

    #  displayManager = {
    #    defaultSession = "none+xmonad";
    #    lightdm.greeters.mini = {
    #      enable = true;
    #      user = "ebn";
    #      extraConfig = ''
    #        [greeter-theme]
    #        background-image = "";
    #        background-color = "#0C0F12"
    #        text-color = "#ff79c6"
    #        password-background-color = "#1E2029"
    #        window-color = "#181a23"
    #        border-color = "#bd93f9"
    #      '';
    #    };
    #  };

    #  windowManager.xmonad.enable = true;
    #};
  };
}
