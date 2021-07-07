{
  services.picom = {
    enable = true;
    experimentalBackends = true;
    activeOpacity = "1.0";
    inactiveOpacity = "1.0";
    backend = "glx";
    fade = true;
    fadeDelta = 5;
    opacityRule = [
      "95:class_g = 'kitty'"
    ];
    shadow = true;
    shadowOpacity = "0.75";
    blur = true;
    extraOptions = ''
      unredir-if-possible = false;
      blur-strength = 12;
      shadowRadius = 7;
    '';
  };
}
