pkgs:

pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "ebn";
    version = "1.0.0";
    src = ./.;
}
