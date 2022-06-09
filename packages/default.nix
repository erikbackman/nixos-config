final: prev: {
  ebn = {
    ebn-xmonad = prev.callPackage ./ebn-xmonad {};
    ebn-stumpwm = prev.callPackage ./ebn-stumpwm {};
    aurora-vim = prev.callPackage ./aurora-vim {};
    modus-theme-vim = prev.callPackage ./modus-theme-vim {};
    pop-shell = prev.callPackage ./pop-shell {};
    material-design-iconic = prev.callPackage ./material-design-iconic {};
    ebn-core-el = prev.callPackage ./ebn-core.el { trivialBuild = prev.emacsPackages.trivialBuild; };
    iosevka-custom = prev.callPackage ./iosevka-custom {};
    kaolin-themes = prev.callPackage ./emacs-kaolin-themes { trivialBuild = prev.emacsPackages.trivialBuild; };
  };
  zathura = prev.callPackage ./zathura { pkgs = prev; };
}
