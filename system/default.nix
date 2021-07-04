{ pkgs, xmonad, ... }: {
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
    plymouth.enable = true;
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
    feh
    git
    htop
    neofetch
    mesa
    openjdk
    docker
    docker-compose
    sbcl
    xorg.xev
    mpc_cli
    scrot
    streamlink
    mpv
    obs-studio
    ispell
    openshot-qt
    ncpamixer
    orchis
    numix-gtk-theme
    dracula-theme
    libgccjit
    xmobar
    xdotool
    dunst
    claws-mail-gtk3
    gnome3.zenity
    yad
    pass
  ];

  programs.steam.enable = true;

  fonts.fonts = with pkgs; [
    hack-font
    victor-mono
    iosevka
    fantasque-sans-mono
    ubuntu_font_family
    jetbrains-mono
    etBook
    source-sans-pro
    nerdfonts
    google-fonts
    oldstandard
  ];

  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    extraConfig =
      "load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1"; # Needed by mpd
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
            text-color = "#ff79c6"
            password-background-color = "#1E2029"
            window-color = "#181a23"
            border-color = "#181a23"
          '';
        };
      };
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = hp:
          with hp; [
            xmonad-contrib
            dbus
            monad-logger
            X11
          ];
      };
    };

    # Music daemon
    mpd.enable = true;
    mpd.extraConfig = ''
      audio_output {
        type "pulse"
        name "PulseAudio"
        server "127.0.0.1"
      }
    '';
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
    ];

    binaryCachePublicKeys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" # Haskell.nix
    ];

    package = pkgs.nixFlakes;
    # Enable the nix 2.0 CLI and flakes support feature-flags
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  system.stateVersion = "20.03";
}
