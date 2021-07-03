{ pkgs }:

with pkgs;
let
  ghc' = haskellPackages.ghcWithPackages
    (hp: with hp; [ X11 X11-xft dbus utf8-string ]);
  buildInputs = [
    pkg-config
    haskellPackages.haskell-language-server
    ghc'
    cabal-install
    xorg.libX11
    gcc
    rnix-lsp
  ];
in
mkShell {
  inherit buildInputs;
}
