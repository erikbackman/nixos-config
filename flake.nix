{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixpkgs-unstable.url = "nixpkgs/master";
    home-manager.url = "github:rycee/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # Extras
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = inputs@{
      self
    , nixpkgs
    , nixpkgs-unstable
    , home-manager
    , emacs-overlay
    , ...
  }:
    let system = "x86_64-linux";
    in {
      devShell.${system} = import ./shell.nix {
        pkgs = import nixpkgs { inherit system; };
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
