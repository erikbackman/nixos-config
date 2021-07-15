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
      "90:class_g = 'kitty'"
    ];
    shadow = true;
    shadowOpacity = "1"; # def: 0.75
    blur = true;
    extraOptions = ''
      shadow-offset = -14;
      shadow-radius = 20;
      unredir-if-possible = false;
      blur-strength = 12;
    '';
  };
}
