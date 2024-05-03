{ config, pkgs, ...}:

let
  downloadDir = "$HOME/Downloads";
  killall = "${pkgs.psmisc}/bin/killall";
  kodi = "${pkgs.kodi}/bin/kodi";
  pgrep = "${pkgs.procps}/bin/pgrep";
  find = "${pkgs.findutils}/bin/find";
  xargs = "${pkgs.findutils}/bin/xargs";
  secrets = import ../nixos-shared/secrets.nix;
  rm = "${pkgs.coreutils}/bin/rm";
  sendIpAddr = "${pkgs.myScripts.sendIpAddr secrets.telegramBotToken}/bin/sendIpAddr";
  telegramSendPhoto = "${pkgs.myScripts.telegramSendPhoto secrets.telegramBotToken}/bin/telegramSendPhoto";
  userName = config.lib._custom_.userName;
  curl = "${pkgs.curl}/bin/curl";
in
{
  services = {
    cron = {
      enable = true;
      systemCronJobs = [
        " 0           3 * *   * ${userName} ${killall} kodi kodi.bin kodi-x11 2>/dev/null"
        " 5           3 * *   * ${userName} ${killall} -9 kodi kodi.bin kodi-x11 2>/dev/null"
        " 0          20 * *   * ${userName} ${pgrep} kodi || env KODI_AE_SINK=ALSA DISPLAY=:0 XDG_RUNTIME_DIR=/run/user/1000 ${kodi} &>/dev/null &"
        " 0           * * *   * ${userName} ${find} ${downloadDir} -type f \\( -iname '*.idx' -or -iname '*.sfv' -or -iname '*.url' -or -iname '*.rev' -or -iname '*[.-]sample.[ma][kv][vi]' \\) -delete"
        " 5  4,10,16,22 * *   * ${userName} ${find} ${downloadDir} -mindepth 2 -type f \\( -iname \"*.mkv\" -or -iname \"*.avi\" \\) -exec mv -v {} ${downloadDir} ';'"
        "35           4 * *   * ${userName} ${find} ${downloadDir} -type d -empty -delete"
        " 0   0/5     * * *   * ${userName} ${curl} -s https://hc-ping.com/6656b215-0e49-48ff-9af0-a79c64faab9f"
        " 0   0       7 * *   * ${userName} ${find} /media/backups/Photos/web -name \"*$(date -d '-1 year' +%Y%m%d)*\" | ${xargs} ${telegramSendPhoto}"
      ];
    };
  };
}
