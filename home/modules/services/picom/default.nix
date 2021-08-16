{
  services.picom = {
    enable = true;
    experimentalBackends = false;
    activeOpacity = "1.0";
    inactiveOpacity = "1.0";
    backend = "glx";
    fade = true;
    fadeDelta = 4;
    # opacityRule = [
    #   "95:class_g = 'kitty'"
    # ];
    shadow = true;
    shadowOpacity = "1"; # def: 0.75
    blur = false;
    extraOptions = ''
      shadow-offset = -14;
      shadow-radius = 20;
      unredir-if-possible = false;
      blur-strength = 12;
    '';
  };
}
