{ pkgs, ... }:

pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "modus-theme-vim";
    version = "1.0.0";
    src = pkgs.fetchFromGitHub {
      owner = "ishan9299";
      repo = "modus-theme-vim";
      rev = "6b82be8589a590405c1347f17402d845e52d776f";
      sha256 = "1qyw5n9ig8pj5wnh2ic7ap897ix6nzg00cn6mcmw91aryzmgpi25";
    };
    meta.homepage = "https://github.com/ishan9299/modus-theme-vim";
}
