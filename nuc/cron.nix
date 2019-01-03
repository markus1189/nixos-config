userName:
{ config, pkgs, ...}:

let
  killall = "${pkgs.psmisc}/bin/killall";
  kodi = "${pkgs.kodi}/bin/kodi";
  pgrep = "${pkgs.procps}/bin/pgrep";
in
{
  services = {
    cron = {
      enable = true;
      systemCronJobs = [
        "30 15 * * thu ${userName} bash ~/in-or-out.sh"
        " 0  3 * * * ${userName} ${killall} kodi 2>/dev/null"
        " 0  3 * * * ${userName} ${killall} kodi.bin 2>/dev/null"
        " 5  3 * * * ${userName} ${killall} -9 kodi 2>/dev/null"
        " 5  3 * * * ${userName} ${killall} -9 kodi.bin 2>/dev/null"
        "10 20 * * * ${userName} ${pgrep} kodi || DISPLAY=:0 ${kodi} &>/dev/null &"
      ];
    };
  };
}
