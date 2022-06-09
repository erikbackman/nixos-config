{pkgs, lib, stdenv, lispPackages, ... }:

stdenv.mkDerivation rec {
  pname = "ebn-stumpwm";
  version = "0.0.1";
  unpackPhase = "true";

  # FIXME propagatedInputs?
  buildInputs = [ lispPackages.stumpwm
                  lispPackages.clwrapper
                  lispPackages.swank
                  lispPackages.asdf
                  lispPackages.clx-truetype ];

  buildPhase = ''
  '';

  installPhase = ''
    mkdir -p $out/bin
    script="$out"/bin/ebn-stumpwm.sh
    mkdir -p "$(dirname "$script")"
    touch "$script"
    chmod a+x "$script"

    echo "#! ${stdenv.shell}" >> "$script"

    echo "source ${pkgs.lispPackages.stumpwm}/lib/common-lisp-settings/stumpwm-shell-config.sh" >> "$script"
    echo "source ${pkgs.lispPackages.clx-truetype}/lib/common-lisp-settings/clx-truetype-shell-config.sh" >> "$script"
    echo "source ${pkgs.lispPackages.swank}/lib/common-lisp-settings/swank-shell-config.sh" >> "$script"
    echo "source ${pkgs.lispPackages.asdf}/lib/common-lisp-settings/swank-shell-config.sh" >> "$script"

    echo '"${pkgs.lispPackages.clwrapper}/bin/common-lisp.sh" --quit --eval "(require :stumpwm)" --eval "(stumpwm:stumpwm)"' >> "$script"
  '';

  meta = with lib; {
    homepage = https://github.com/stumpwm/stumpwm;
    description = "A wrapper to use common-lisp.sh to launch stumpwm";
    license = licenses.gpl2;
    maintainers = [ maintainers.ebn ];
    platforms = platforms.unix;
  };
}
