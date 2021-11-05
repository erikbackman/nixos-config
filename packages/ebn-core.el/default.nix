{ lib
, trivialBuild
, fetchFromGitHub
, emacs
}:

trivialBuild rec {
  pname = "ebn-core-el";
  version = "0.1";

  src = ./src;

  buildInputs = [
    emacs
  ];

  meta = with lib; {
    maintainers = [ ebn ];
    inherit (emacs.meta) platforms;
  };
}
