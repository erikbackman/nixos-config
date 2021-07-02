{
  description = "My XMonad";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    devshell.url = "github:numtide/devshell/master";
  };

  outputs = { self, nixpkgs, flake-utils, devshell }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ devshell.overlay ];
        };
        ghc' = pkgs.haskellPackages.ghcWithPackages
          (hp: with hp; [ X11 X11-xft dbus utf8-string ]);
        tooling = with pkgs; [
          pkg-config
          haskellPackages.haskell-language-server
          ghc'
          cabal-install
          xorg.libX11
          gcc
          rnix-lsp
        ];
      in {
        devShell = pkgs.devshell.mkShell {
          name = "xmonad-shell";
          packages = tooling;
        };

        package.xmonad = pkgs.haskell.packages.ghc884.callPackage ./default.nix { };
        defaultPackage = pkgs.haskell.packages.ghc884.callPackage ./default.nix { };
      });
}
