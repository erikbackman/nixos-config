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
      # "100:name *= 'i3lock'"
      # "100:class_g = 'Chromium-browser'"
      # "100:class_g = 'Emacs'"
      # "100:class_g = 'Nyxt'"
      # "85:class_g = 'Emacs'"
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
