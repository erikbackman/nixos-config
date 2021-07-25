{ pkgs, ... }: {
  imports = [ ./hardware-configuration.nix ];

  users.users.ebn = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "docker" "audio" "video" ];
    shell = pkgs.bash;
  };

  # Forgive me Saint Stallman for I have sinned.. and fuck you nvidia.
  nixpkgs.config.allowUnfree = true;

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  networking = {
    hostName = "nixos";
    firewall.enable = true;
    #firewall.allowedTCPPorts = [ ... ];
    #firewall.allowedUDPPorts = [ ... ];
  };

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "sv-latin1";
  };

  time.timeZone = "Europe/Stockholm";

  environment.systemPackages = with pkgs; [
    wget
    vim
    git
    # mesa
    xorg.xev
    ispell
    ncpamixer
    pass
    cachix
  ];

  programs.steam.enable = true;

  fonts.fonts = with pkgs; [
    hack-font
    iosevka
    jetbrains-mono
    etBook
    oldstandard
    google-fonts
    siji
  ];

  sound.enable = true;
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

      # monitorSection = ''
      #     VendorName     "Unknown"
      #     ModelName      "Samsung C34H89x"
      #     HorizSync       30.0 - 152.0
      #     VertRefresh     50.0 - 100.0
      #     Option         "DPMS"
      # '';
      # screenSection = ''
      #   Option         "Stereo" "0"
      #   Option         "metamodes" "3440x1440_100 +0+0"
      #   Option         "SLI" "Off"
      #   Option         "MultiGPU" "Off"
      #   Option         "BaseMosaic" "off"
      # '';

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

      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };

    };
    # Music daemon
    # mpd = {
    #   enable = true;
    #   extraConfig = ''
    #     audio_output {
    #       type "pulse"
    #       name "PulseAudio"
    #       server "127.0.0.1"
    #    }
    #   '';
    # };
  };

  # Docker
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
