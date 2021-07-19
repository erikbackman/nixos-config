{
  description = "ebn nixos configuration";

  nixConfig.bash-prompt = "❄ nix-develop $ ";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixpkgs-unstable.url = "nixpkgs/master";
    home-manager.url = "github:rycee/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # Extras
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = inputs@{ nixpkgs, home-manager, emacs-overlay, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ emacs-overlay.overlay ];
      };
    in {
      devShell.${system} = import ./shell.nix { inherit pkgs; };

      packages.${system} = {
        xmonad = pkgs.haskell.packages.ghc884.callPackage
          ./home/modules/windowManager/xmonad/package/ebn-xmonad/xmonad.nix { };
      };

      nixosConfigurations = {
        nixos = nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = { inherit inputs; };
          modules = [
            ./system
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = false;
              home-manager.useUserPackages = true;
              home-manager.extraSpecialArgs = { inherit inputs; };
              home-manager.users.ebn = import ./home;
            }
          ];
        };
      };
    };
}
