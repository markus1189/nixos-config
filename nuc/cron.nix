userName:
{ config, pkgs, ...}:

let
  downloadDir = "$HOME/Downloads";
  killall = "${pkgs.psmisc}/bin/killall";
  pgrep = "${pkgs.procps}/bin/pgrep";
  find = "${pkgs.findutils}/bin/find";
  secrets = import ../nixos-shared/secrets.nix;
  rm = "${pkgs.coreutils}/bin/rm";
  sendIpAddr = "${pkgs.myScripts.sendIpAddr secrets.telegramBotToken}/bin/sendIpAddr";
in
{
  services = {
    cron = {
      enable = true;
      systemCronJobs = [
        " 5  4,10,16,22 * *   * ${userName} ${find} ${downloadDir} -mindepth 2 -type f \\( -iname \"*.mkv\" -or -iname \"*.avi\" \\) -exec mv -v {} ${downloadDir} ';'"
        "35           4 * *   * ${userName} ${find} ${downloadDir} -type d -empty -delete"
        "0 5 * * * ${userName} ${sendIpAddr}"
      ];
    };
  };
}
