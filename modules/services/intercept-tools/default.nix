{ config, lib, pkgs, ... }: {
  services.interception-tools = {
    enable = true;
    plugins = with pkgs.interception-tools-plugins;
      [ dual-function-keys ];
    udevmonConfig =
      let mappings =
            pkgs.writeText "dual-function-keys.yaml" ''
            TIMING:
              TAP_MILLISEC: 200
              DOUBLE_TAP_MILLISEC: 150

            MAPPINGS:
              - KEY: KEY_ENTER
                TAP: KEY_ENTER
                HOLD: KEY_LEFTALT
            '';
          uinput = "${pkgs.interception-tools}/bin/uinput";
          intercept = "${pkgs.interception-tools}/bin/intercept";
          dual-function-keys = "${pkgs.interception-tools-plugins.dual-function-keys}/bin/dual-function-keys";
      in ''
         - JOB: "${intercept} -g $DEVNODE | ${dual-function-keys} -c ${mappings} | ${uinput} -d $DEVNODE"
           DEVICE: 
             EVENTS:
               EV_KEY: [KEY_ENTER]
             LINK: .*-event-kbd
      '';
  };
}
