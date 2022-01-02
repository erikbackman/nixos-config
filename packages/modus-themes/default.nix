{ lib
, trivialBuild
, fetchFromGitHub
, emacs
, emacsPackages
, pkgs
}:

emacsPackages.melpaBuild rec {
  pname = "modus-themes";
  version = "1.6.4";

  commit = "1cff9f114526505b621111d7d68294c1d269bada";
  
  src = fetchFromGitHub {
    owner = "erikbackman";
    repo = "modus-themes";
    rev = "2f5b7ccbc4d13500344b1e5c9fe1be88d3ca964b";
    sha256 = "169ns1gy58a4gx3zl8kzg6ibxwr0vrfx2k6svyhyfv0xd1z0h404";
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
    (modus-themes
    :repo "erikbackman/emacs-modus-themes"
    :fetcher github
    :branch "main")
  '';

  meta = with lib; {
    maintainers = [ ebn ];
    inherit (emacs.meta) platforms;
  };
}
