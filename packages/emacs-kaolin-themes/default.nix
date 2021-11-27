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

  commit = "1cff9f114526505b621111d7d68294c1d269bada";
  
  src = fetchFromGitHub {
    owner = "erikbackman";
    repo = "emacs-kaolin-themes";
    rev = "1cff9f114526505b621111d7d68294c1d269bada";
    sha256 = "1dyd9yc91ljb8ia46b7yh0zf50f1cz8kql9m6nmq9981djf1wj64";
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
