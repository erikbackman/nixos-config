# modules/dev/cc.nix --- C & C++
{ config, lib, pkgs, ... }:

with lib;
let cfg = config.ebn.dev.cc;
in {
  options.ebn.dev.cc = {
    enable = mkEnableOption "Enable C/C++ development";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      clang
      # gcc
      bear
      gdb
      cmake
      llvmPackages.libcxx
    ];
  };
}
