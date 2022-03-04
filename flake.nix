{
  description = "ebn nixos configuration";
  
  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    nixpkgs-unstable.url = "nixpkgs/master";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    neovim-git.url = "github:neovim/neovim?dir=contrib";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
  };

  outputs = inputs@{ self, nixpkgs, nixpkgs-unstable, emacs-overlay, neovim-git, nixos-hardware, ... }:
    let
      system = "x86_64-linux";
      make-packages = ps: attrs:
        import ps ({
          inherit system;
          config.allowUnfree = true;
        } // attrs);

      pkgs = make-packages nixpkgs {
        overlays = [
          emacs-overlay.overlay
          neovim-git.overlay
          (import ./packages)
          (final: prev: { steam = (make-packages nixpkgs-unstable { }).steam; })
        ];
      };

      my-lib = import ./lib/lib.nix pkgs.lib;
      my-modules = my-lib.listModulesRec ./modules;
      
      make-system = extra-modules: nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = { inherit inputs pkgs; };
        modules = pkgs.lib.lists.flatten (extra-modules ++ my-modules);
      };
      
    in {
      devShell.${system} = import ./shell.nix { inherit pkgs; };
      
      nixosConfigurations = {

        yggdrasil = make-system [
            ./hosts/base
            ./hosts/yggdrasil
            ./hosts/yggdrasil/hardware-configuration.nix
        ];

        bifrost = make-system [
            ./hosts/base
            ./hosts/bifrost
            ./hosts/bifrost/hardware-configuration.nix
            nixos-hardware.nixosModules.lenovo-thinkpad-t480
        ];
      };
    };
}
