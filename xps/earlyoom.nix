{ config, pkgs, ...}:

{
  services = {
    earlyoom = {
      enable = true;
      enableDebugInfo = true;
      freeMemThreshold = 10;
      freeSwapThreshold = 50;
    };
  };
}
