final: prev: {
  ebn = {
    ebn-xmonad = prev.callPackage ./ebn-xmonad {};
    aurora-vim = prev.callPackage ./aurora-vim {};
    modus-theme-vim = prev.callPackage ./modus-theme-vim {};
    pop-shell = prev.callPackage ./pop-shell {};
    material-design-iconic = prev.callPackage ./material-design-iconic {};
  };
  zathura = prev.callPackage ./zathura { pkgs = prev; };
}
