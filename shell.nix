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

  ebn-xmonad = import ./packages/ebn-xmonad pkgs;
  haskellTooling = [
    cabal-install
    ghcid
    haskellPackages.hindent
    haskellPackages.ormolu
    haskellPackages.haskell-language-server
  ];

in mkShell {
  buildInputs = cLibsAndTools ++ haskellTooling ++ [ rnix-lsp ]
    ++ [ sumneko-lua-language-server ];

  inputsFrom = [ ebn-xmonad.env ];
  shellHook = ''
    alias nrb="nixos-rebuild build --flake ."
    alias nrt="sudo nixos-rebuild test --flake ."
    alias nrs="sudo nixos-rebuild switch --flake ."
  '';
}
