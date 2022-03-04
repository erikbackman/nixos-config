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
      make-pkg-set = ps: attrs:
        import ps ({
          inherit system;
          config.allowUnfree = true;
        } // attrs);

      pkgs = make-pkg-set nixpkgs {
        overlays = [
          emacs-overlay.overlay
          neovim-git.overlay
          (import ./packages)
          (final: prev: { steam = (make-pkg-set nixpkgs-unstable { }).steam; })
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

        yggdrasil = nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = { inherit inputs pkgs; };
          modules = pkgs.lib.lists.flatten [
            ./hosts/base
            ./hosts/yggdrasil
            ./hosts/yggdrasil/hardware-configuration.nix
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
            myModules
            nixos-hardware.nixosModules.lenovo-thinkpad-t480
          ];
        };
      };
    };
}
