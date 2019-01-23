userName:
{ config, pkgs, ...}:

let
  downloadDir = "$HOME/Downloads";
  killall = "${pkgs.psmisc}/bin/killall";
  kodi = "${pkgs.kodi}/bin/kodi";
  pgrep = "${pkgs.procps}/bin/pgrep";
  find = "${pkgs.findutils}/bin/find";
  secrets = import ../nixos-shared/secrets.nix;
  rm = "${pkgs.coreutils}/bin/rm";
in
{
  services = {
    cron = {
      enable = true;
      systemCronJobs = [
        " 0           3 * *   * ${userName} ${killall} kodi 2>/dev/null"
        " 0           3 * *   * ${userName} ${killall} kodi.bin 2>/dev/null"
        " 5           3 * *   * ${userName} ${killall} -9 kodi 2>/dev/null"
        " 5           3 * *   * ${userName} ${killall} -9 kodi.bin 2>/dev/null"
        "10          20 * *   * ${userName} ${pgrep} kodi || DISPLAY=:0 ${kodi} &>/dev/null &"
        " 5  4,10,16,22 * *   * ${userName} ${find} ${downloadDir} -mindepth 2 -type f \\( -iname \"*.mkv\" -or -iname \"*.avi\" \\) -exec mv -v {} ${downloadDir} ';'"
        "35           4 * *   * ${userName} ${find} ${downloadDir} -type d -empty -delete"
      ];
    };
  };
}
