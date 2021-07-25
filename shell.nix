{ pkgs }:

with pkgs;
let
  cLibsAndTools = [
    clang
    zlib
    pkg-config
    xorg.libX11
    xorg.libXinerama
    xorg.libXrandr
    xorg.libXScrnSaver
    xorg.libXext
  ];

  haskellTooling = let
    ghc' = haskellPackages.ghcWithPackages
      (hp: with hp; [ X11 X11-xft dbus utf8-string zlib cpphs ]);
  in [
    ghc'
    cabal-install
    ghcid
    haskellPackages.hindent
    haskellPackages.haskell-language-server
  ];

in mkShell {
  buildInputs = cLibsAndTools ++ haskellTooling;
  shellHook = ''
    alias os-build="nixos-rebuild build --flake ."
    alias os-test="sudo nixos-rebuild test --flake ."
    alias os-switch="sudo nixos-rebuild switch --flake ."
  '';
}
