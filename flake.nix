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

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
  };

  outputs = inputs@{ self, nixpkgs, home-manager, emacs-overlay, neovim-git, nixos-hardware, ... }:
    let
      system = "x86_64-linux";

      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [ 
          emacs-overlay.overlay
          neovim-git.overlay
          (import ./overlays)
        ];
      };

      myLib = import ./lib/lib.nix pkgs.lib;
      myModules = myLib.listModulesRec ./modules;

    in {
      devShell.${system} = import ./shell.nix { inherit pkgs; };

      packages.${system} = { ebn-xmonad = pkgs.ebn.ebn-xmonad; };

      nixosConfigurations = {

        nixos = nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = { inherit inputs pkgs; };
          modules = pkgs.lib.lists.flatten [
            ./hosts/base
            ./hosts/desktop/system
            ./hosts/desktop/system/hardware-configuration.nix
            myModules
          ];
        };

        bifrost = nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = { inherit inputs pkgs; };
          modules = pkgs.lib.lists.flatten [ 
            ./hosts/base
            ./hosts/bifrost/system 
            # TODO: Hardware config
            myModules
            nixos-hardware.nixosModules.lenovo-thinkpad-t480
          ];
        };

      };
    };
}
