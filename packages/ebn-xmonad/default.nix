{ pkgs, ...}: pkgs.haskell.packages.ghc902.callCabal2nix "ebn-xmonad" ./. { }
