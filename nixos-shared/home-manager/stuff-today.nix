# Roll ~/Stuff/Today over to the new day before anything reads it
# (flameshot savePath, rofi-today, /stuff-it), not only when a shell runs cdt.
# Imported by laptop/home.nix and nuc/home.nix.
{ pkgs, ... }:
{
  systemd.user.services.stuff-today = {
    Unit.Description = "Create today's ~/Stuff dir and repoint ~/Stuff/Today";
    Service = {
      Type = "oneshot";
      ExecStart = "${pkgs.myScripts.stuffToday}/bin/stuff-today";
    };
  };

  systemd.user.timers.stuff-today = {
    Unit.Description = "Daily ~/Stuff/Today rollover";
    Timer = {
      OnCalendar = "*-*-* 00:00:05";
      # At user-manager start (login). On the timer rather than a
      # default.target WantedBy on the service, so home-manager activation
      # does not run it; stuff-today's same-day guard makes reruns harmless.
      OnStartupSec = "0";
      # Powered off over midnight; suspend catches up without it.
      Persistent = true;
    };
    Install.WantedBy = [ "timers.target" ];
  };
}
