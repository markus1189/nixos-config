{ config, pkgs, ... }:

{
  boot = {
    kernelParams = [
      # https://bugzilla.redhat.com/show_bug.cgi?id=805285
      # fix for 'Queue <x> is active on fifo 1 and stuck for 10000 ms'
      "iwlwifi.wd_disable=1"
    ];

    # Try to fix iwlwifi issues (again)
    # https://bbs.archlinux.org/viewtopic.php?pid=1918191#p1918191
    extraModprobeConfig = ''
      options iwlmvm power_scheme=1

      options iwlwifi 11n_disable=1
      options iwlwifi swcrypto=0
      options iwlwifi bt_coex_active=0
      options iwlwifi power_save=0
      options iwlwifi d0i3_disable=1
      options iwlwifi uapsd_disable=1
      options iwlwifi lar_disable=1
    '';
  };
}
