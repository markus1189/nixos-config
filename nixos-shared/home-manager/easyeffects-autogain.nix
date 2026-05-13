{ ... }:
{
  services.easyeffects = {
    enable = true;
    preset = "autogain";

    extraPresets.autogain.output = {
      blocklist = [ ];
      plugins_order = [ "autogain#0" ];

      "autogain#0" = {
        bypass = false;
        "input-gain" = 0.0;
        "output-gain" = 0.0;
        target = -23.0;
        reference = "Geometric Mean (MSI)";
        "maximum-history" = 60;
        "silence-threshold" = -70.0;
        "force-silence" = false;
      };
    };
  };
}
