{ pkgs, ... }:

pkgs.iosevka.override {
  privateBuildPlan = ''
        [buildPlans.iosevka-custom]
        family = "Iosevka Custom"
        spacing = "normal"
        serifs = "sans"
        no-cv-ss = true

        [buildPlans.iosevka-custom.variants]
        inherits = "ss08"

        [buildPlans.iosevka-custom.ligations]
        inherits = "haskell"
      '';
  set = "custom";
}
