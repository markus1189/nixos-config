# Global python3, so throwaway scripts don't need a `nix shell --expr` shebang.
#
# All of these are already in the system closure via other consumers (visidata's
# overlay, myScripts, garmin-connect); measured delta on p1g8 2026-08-14 was
# 0.4 MB for this env, plus 52 MB of python docs pulled by documentation.dev.
#
# No pip: it cannot write to the store, and `pip install --user` would shadow
# store packages out of ~/.local.
#
# Keep the copy of this list in claude/CLAUDE-global.md in sync.
{ pkgs, ... }:

{
  environment.systemPackages = [
    (pkgs.python3.withPackages (ps: with ps; [
      beautifulsoup4
      httpx
      lxml
      matplotlib
      numpy
      pandas
      pillow
      psutil
      python-dateutil
      pyyaml
      requests
      rich
    ]))
  ];
}
