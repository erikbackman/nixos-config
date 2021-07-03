{ pkgs }:

with pkgs;
let
  ghc' = pkgs.haskellPackages.ghcWithPackages
    (hp: with hp; [ X11 X11-xft dbus utf8-string ]);
  buildInputs = with pkgs; [
    pkg-config
    haskellPackages.haskell-language-server
    ghc'
    cabal-install
    xorg.libX11
    gcc
    rnix-lsp
  ];
in
pkgs.mkShell {
  inherit buildInputs;
}
