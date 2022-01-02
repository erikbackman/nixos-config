{ pkgs, lib, config, ... }: {
  networking.hostName = "yggdrasil";

  nixpkgs.config.allowUnfree = true;

  ebn.themes = {
    enable = true;
    palette = "aurora";
  };

  programs.steam.enable = true;
  programs.ebn.nvim.enable = true;
  programs.ebn.kitty.enable = true;
  programs.ebn.rofi.enable = true;
  programs.ebn.bash.enable = true;
  programs.ebn.bash.starship.enable = true;
  programs.ebn.emacs.enable = true;
  programs.ebn.emacs.package = pkgs.emacsGcc;

  services.redshift = {
    enable = true;
    temperature.night = 4000;
    inherit (config location);
  };

  windowManager.ebn.xmonad.enable = true;
  services.ebn.polybar.enable = true;
  services.xserver.enable = true;
  services.xserver.displayManager.sessionCommands = ''
    xrandr --output DP-0 --mode 3440x1440 --rate 99.98
    xset r rate 150 25
  '';

  services.ebn.syncthing.enable = true;
  services.ebn.pulseeffects.enable = true;
  services.xserver.videoDrivers = [ "nvidia" ];
  services.xserver.layout = "se";
  services.fstrim.enable = true;

  services.interception-tools = {
    enable = true;
    plugins = with pkgs.interception-tools-plugins;
      [ dual-function-keys ];
    udevmonConfig =
      let mappings =
            pkgs.writeText "dual-function-keys.yaml" ''
            TIMING:
              TAP_MILLISEC: 200
              DOUBLE_TAP_MILLISEC: 150

            MAPPINGS:
              - KEY: KEY_ENTER
                TAP: KEY_ENTER
                HOLD: KEY_LEFTALT
            '';
          uinput = "${pkgs.interception-tools}/bin/uinput";
          intercept = "${pkgs.interception-tools}/bin/intercept";
          dual-function-keys = "${pkgs.interception-tools-plugins.dual-function-keys}/bin/dual-function-keys";
      in ''
         - JOB: "${intercept} -g $DEVNODE | ${dual-function-keys} -c ${mappings} | ${uinput} -d $DEVNODE"
           DEVICE: 
             EVENTS:
               EV_KEY: [KEY_ENTER]
             LINK: .*-event-kbd
      '';
  };

  fonts.fonts = with pkgs; [
    etBook
    google-fonts
    hack-font
    ebn.iosevka-custom
    jetbrains-mono
    victor-mono
    oldstandard
    gyre-fonts
    libre-baskerville
    siji
    cm_unicode
    bakoma_ttf
    lmmath
    proggyfonts
    tamsyn
    julia-mono
  ];

  fonts.fontconfig = {
    enable = true;
    antialias = true;
    hinting = {
      enable = true;
    };
  };

  environment.systemPackages = with pkgs; [
    zathura
    maxima
    wxmaxima
    texlive.combined.scheme-full
    libreoffice
    firefox
    playerctl
    gsettings-desktop-schemas
    gnumake
    obs-studio
    neofetch
    mpc_cli
    mpv
    streamlink
    feh
    #discord
    gnome.gucharmap
    gimp
    rofi-pass
    kdenlive
    cowsay
    ncspot
    pavucontrol
    #anki
    sage
    #geogebra6
    jekyll
    man-pages
    brave
    trash-cli
  ] 
  ++ 
  lib.lists.optional (! config.desktopEnvironment.ebn.gnome.enable) claws-mail;

  system.stateVersion = "20.03";
}
