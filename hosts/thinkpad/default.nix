{ pkgs, ... }: {
  imports = [ 
    ../../base-system.nix
  ];

  networking = {
    hostName = "thinkpad";
    firewall.enable = true;
  };

  programs.steam.enable = true;

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

    xserver = {
      enable = true;
      layout = "se";

      displayManager = {
        defaultSession = "none+xmonad";
        lightdm.greeters.mini = {
          enable = true;
          user = "ebn";
          extraConfig = ''
            [greeter-theme]
            background-image = "";
            background-color = "#0C0F12"
            text-color = "#ff79c6"
            password-background-color = "#1E2029"
            window-color = "#181a23"
            border-color = "#bd93f9"
          '';
        };
      };

      windowManager.xmonad.enable = true;
    };
  };
}
