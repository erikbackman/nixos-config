{
  description = "ebn nixos configuration";

  nixConfig.bash-prompt = "❄ nix-develop $ ";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixpkgs-unstable.url = "nixpkgs/master";

    # Extras
    home-manager.url = "github:rycee/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    neovim-git.url = "github:neovim/neovim?dir=contrib";
  };

  outputs = inputs@{ nixpkgs, home-manager, emacs-overlay, neovim-git, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays =
          [ emacs-overlay.overlay neovim-git.overlay (import ./overlays) ];
      };
    in {
      devShell.${system} = import ./shell.nix { inherit pkgs; };

      packages.${system} = { ebn-xmonad = pkgs.ebn.ebn-xmonad; };

      nixosConfigurations = {
        nixos = nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = { inherit inputs pkgs; };
          modules = [
            ./hosts/desktop/system
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = false;
              home-manager.useUserPackages = true;
              home-manager.extraSpecialArgs = { inherit inputs pkgs; hm-modules-path = ./hm-modules; };
              home-manager.users.ebn = import ./hosts/desktop/home;
            }
          ];
        };
      };
    };
}
