{ config, pkgs, ...}:

let
  killall = "${pkgs.psmisc}/bin/killall";
  kodi = "${pkgs.kodi}/bin/kodi";
  pgrep = "${pkgs.procps}/bin/pgrep";
in
{
  services = {
    cron = {
      systemCronJobs = [
        " 0  3 * * * ${killall} kodi 2>/dev/null"
        " 0  3 * * * ${killall} kodi.bin 2>/dev/null"
        " 5  3 * * * ${killall} -9 kodi 2>/dev/null"
        " 5  3 * * * ${killall} -9 kodi.bin 2>/dev/null"
        "10 20 * * * ${pgrep} kodi || DISPLAY=:0 ${kodi} &>/dev/null &"
      ];
    };
  };
}
