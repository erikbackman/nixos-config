{ lib
, trivialBuild
, fetchFromGitHub
, emacs
, emacsPackages
, pkgs
}:

emacsPackages.melpaBuild rec {
  pname = "kaolin-themes";
  version = "1.6.4";

  commit = "c5544e32ad799ad8d7204207e681f47a77c7b1e1";
  
  src = fetchFromGitHub {
    owner = "erikbackman";
    repo = "emacs-kaolin-themes";
    rev = "c5544e32ad799ad8d7204207e681f47a77c7b1e1";
    sha256 = "1dv7hdhchgfz7siqi6m15ljwjqwn8abpqch306rp8w0678yd8p0p";
  };

  buildInputs = [
    emacs
    emacsPackages.cl-lib
    emacsPackages.autothemer
  ];

  packageRequires = [
    emacsPackages.cl-lib
    emacsPackages.autothemer
  ];

  recipe = pkgs.writeText "recipe" ''
    (kaolin-themes
    :repo "erikbackman/emacs-kaolin-themes"
    :fetcher github
    :files ("*.el" "themes/*.el"))
  '';

  meta = with lib; {
    maintainers = [ ebn ];
    inherit (emacs.meta) platforms;
  };
}
