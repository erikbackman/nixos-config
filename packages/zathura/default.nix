{ pkgs, ... }:

let config = pkgs.writeTextDir "zathurarc" (builtins.readFile ./config/zathurarc);
    wrapped = pkgs.writeShellScriptBin "zathura" ''exec ${pkgs.zathura}/bin/zathura -c ${config.outPath} $@'';
    package = pkgs.symlinkJoin rec {
      name = "zathura";
      paths = [
        wrapped
        pkgs.zathura
      ];
    };
in package
