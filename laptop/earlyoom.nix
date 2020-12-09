{ config, pkgs, ...}:

{
  services = {
    earlyoom = {
      enable = false;
      enableDebugInfo = true;
      freeMemThreshold = 10;
      freeSwapThreshold = 50;
    };
  };
}
