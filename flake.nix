{
  description = "ebn nixos configuration";

  nixConfig.bash-prompt = "❄ nix-develop $ ";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    nixpkgs-unstable.url = "nixpkgs/master";

    # Extras
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    neovim-git.url = "github:neovim/neovim?dir=contrib";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
  };

  outputs = inputs@{ self, nixpkgs, nixpkgs-unstable, emacs-overlay, neovim-git, nixos-hardware, ... }:
    let
      system = "x86_64-linux";

      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [ 
          emacs-overlay.overlay
          neovim-git.overlay
          (import ./packages)
          (final: prev: { 
            steam = 
              let up = import nixpkgs-unstable { inherit system; config.allowUnfree = true; };
              in up.steam;
          })
        ];
      };

      myLib = import ./lib/lib.nix pkgs.lib;
      myModules = myLib.listModulesRec ./modules;

    in {

      # Output pkgs and myLib so I can access them via builtins.getFlake in a nix repl
      inherit pkgs;
      inherit myLib;

      devShell.${system} = import ./shell.nix { inherit pkgs; };

      packages.${system} = { ebn-xmonad = pkgs.ebn.ebn-xmonad; };

      nixosConfigurations = {

        nixos = nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = { inherit inputs pkgs; };
          modules = pkgs.lib.lists.flatten [
            ./hosts/base
            ./hosts/desktop
            ./hosts/desktop/hardware-configuration.nix
            myModules
          ];
        };

        bifrost = nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = { inherit inputs pkgs; };
          modules = pkgs.lib.lists.flatten [ 
            ./hosts/base
            ./hosts/bifrost
            ./hosts/bifrost/hardware-configuration.nix
            # TODO: Hardware config
            myModules
            nixos-hardware.nixosModules.lenovo-thinkpad-t480
          ];
        };

      };
    };
}
