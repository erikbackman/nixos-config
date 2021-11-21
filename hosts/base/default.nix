{ pkgs, lib, ... }: {

  users.users.ebn = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "audio" "video" ];
    shell = pkgs.bash;
  };

  boot = {
    kernelPackages = pkgs.linuxPackages_5_14;
    loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot.configurationLimit = 10;
      systemd-boot.enable = true;
    };
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "sv-latin1";
  };

  time.timeZone = "Europe/Stockholm";
  i18n.defaultLocale = "en_US.UTF-8";

  location = { latitude = 59.4022; longitude = 13.5115; };

  environment.systemPackages = with pkgs; [
    cachix
    git
    ispell
    ncpamixer
    (pass.withExtensions (exts: [ exts.pass-import ]))
    w3m
    wget
    nnn
  ];

  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    extraConfig = ''
      load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1; ### Needed by mpd
    '';
  };

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
      #"https://hydra.iohk.io" # Haskell.nix
      "https://ebn.cachix.org"
    ];

    binaryCachePublicKeys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      #"hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" # Haskell.nix
      "ebn.cachix.org-1:Z8QlCh2aXXURAyMpMgDh7VaHo/iTJ7P8r52fxpjJ5JA="
    ];

    package = pkgs.nixFlakes;
    # Enable the nix 2.0 CLI and flakes support feature-flags
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
}
