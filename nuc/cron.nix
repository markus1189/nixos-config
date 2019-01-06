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
  in-or-out = import ./in-or-out.nix {
    inherit (userPkgs) writeShellScriptBin;
    inherit (pkgs) curl;
    channel = "#ffm";
    token = secrets.ffmBotToken;
  };
  userPkgs = import ../nixos-shared/scripts/scripts.nix { inherit pkgs; };
in
{
  services = {
    cron = {
      enable = true;
      systemCronJobs = [
        "30          15 * * thu ${userName} ${in-or-out}/bin/in-or-out"
        " 0           3 * *   * ${userName} ${killall} kodi 2>/dev/null"
        " 0           3 * *   * ${userName} ${killall} kodi.bin 2>/dev/null"
        " 5           3 * *   * ${userName} ${killall} -9 kodi 2>/dev/null"
        " 5           3 * *   * ${userName} ${killall} -9 kodi.bin 2>/dev/null"
        "10          20 * *   * ${userName} ${pgrep} kodi || DISPLAY=:0 ${kodi} &>/dev/null &"
        " 0           * * *   * ${userName} ${find} ${downloadDir} -type f \\( -iname '*.idx' -or -iname '*.nfo' -or -iname '*.sfv' -or -iname '*.url' -or -iname '*.rev' -or -iname '*[.-]sample.[ma][kv][vi]' \\) -delete"
        " 5  4,10,16,22 * *   * ${userName} ${find} ${downloadDir} -mindepth 2 -type f \\( -iname \"*.mkv\" -or -iname \"*.avi\" \\) -exec mv -v {} ${downloadDir} ';'"
        "35           4 * *   * ${userName} ${find} ${downloadDir} -type d -empty -delete"
      ];
    };
  };
}
