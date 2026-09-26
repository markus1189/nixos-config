# ~/Stuff, the date-organized note tree, set up the same way on every host
# that imports this (laptop/home.nix, nuc/home.nix). The tree's content is
# per machine; its tooling and agent instructions come from here, so the
# hosts cannot drift apart. Generated indexes (llms.txt, INDEX.md,
# .kb/series/) are data and stay in ~/Stuff, written by kb-index.
{ pkgs, ... }:
{
  home.packages = [
    pkgs.myScripts.kbIndex
    pkgs.myScripts.kbRetroScan
    pkgs.myScripts.backupStuff
    # For agents navigating the tree by hand (see AGENTS.md); kb-index
    # carries its own copies via runtimeInputs.
    pkgs.treemd
    pkgs.fd
  ];

  # Store symlinks, read-only: change them here, not in ~/Stuff. force,
  # because each host had its own hand-edited copy in the way.
  home.file."Stuff/AGENTS.md" = {
    source = ./AGENTS.md;
    force = true;
  };
  home.file."Stuff/CLAUDE.md" = {
    text = "@AGENTS.md\n";
    force = true;
  };

  # Roll ~/Stuff/Today over to the new day before anything reads it
  # (flameshot savePath, rofi-today, /stuff-it), not only when a shell runs cdt.
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
