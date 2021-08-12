pkgs:

pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "spaceduck";
    version = "1.0.0";
    src = pkgs.fetchFromGitHub {
      owner = "erikbackman";
      repo = "spaceduck";
      rev = "ead8e5ded3d64216e4145160cd79725911f4ac61";
      sha256 = "1vzipbx9yr770fqgjavxzkhb31kg96j6mrl4m3ka6vhfv3fz6p9s";
    };
    meta.homepage = "https://github.com/erikbackman/spaceduck";
}
