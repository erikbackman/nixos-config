{ pkgs }:

with pkgs;
let
  clibsAndTools = [
    clang
    zlib
    pkg-config
    xorg.libX11
    xorg.libXinerama
    xorg.libXrandr
    xorg.libXScrnSaver
    xorg.libXext
  ];

  languageServers = [ haskellPackages.haskell-language-server rnix-lsp ];

  haskellTooling = let
    ghc' = haskellPackages.ghcWithPackages
      (hp: with hp; [ X11 X11-xft dbus utf8-string zlib cpphs ]);
  in [ ghc' cabal-install ghcid ];

in mkShell { buildInputs = clibsAndTools ++ languageServers ++ haskellTooling; }
