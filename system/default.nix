{ pkgs, ... }: {
  imports = [ ./hardware-configuration.nix ];

  users.users.ebn = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "docker" "audio" "video" ];
    shell = pkgs.bash;
  };

  nixpkgs.config.allowUnfree = true;

  boot = {
    loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot.configurationLimit = 10;
      systemd-boot.enable = true;
    };
  };

  networking = {
    hostName = "nixos";
    firewall.enable = true;
  };

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "sv-latin1";
  };

  time.timeZone = "Europe/Stockholm";

  environment.systemPackages = with pkgs; [
    cachix
    git
    ispell
    ncpamixer
    (pass.withExtensions (exts: [ exts.pass-import ]))
    w3m
    wget
    xorg.xev
    nnn
  ];

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

  sound.enable = true;
  programs.dconf.enable = true;
  hardware.pulseaudio = {
    enable = true;
    extraConfig = ''
      load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1; ### Needed by mpd
    '';
  };

  services = {

    dbus = {
      enable = true;
      packages = [ pkgs.gnome3.dconf ];
    };

    xserver = {
      enable = true;
      layout = "se";
      videoDrivers = [ "nvidia" ];

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

  virtualisation.docker.enable = true;

  nix = {
    gc = {
      automatic = true;
      dates = "monthly";
      options = "--delete-older-than 90d";
    };
    autoOptimiseStore = true;

    trustedUsers = [ "ebn" "root" ];

    binaryCaches = [
      "https://nix-community.cachix.org/"
      "https://hydra.iohk.io" # Haskell.nix
      "https://ebn.cachix.org"
    ];

    binaryCachePublicKeys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" # Haskell.nix
      "ebn.cachix.org-1:Z8QlCh2aXXURAyMpMgDh7VaHo/iTJ7P8r52fxpjJ5JA="
    ];

    package = pkgs.nixFlakes;
    # Enable the nix 2.0 CLI and flakes support feature-flags
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  system.stateVersion = "20.03";
}
