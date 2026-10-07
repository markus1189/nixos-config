# Claude Code mods (function-hook plugins), one derivation per mod. The
# claude-code home-manager module puts their store paths on
# CLAUDE_CODE_PLUGIN_DIRS; `checks.claude-mods` builds them all.
#
# The build is the gate: `claude plugin validate` and `claude plugin test`
# run inside it, so a mod whose tests fail never reaches a host. Both run
# offline with an empty HOME. `claude-code` is the one the hosts install,
# so the tests run against the engine the mod will run under.
#
# `marginal` is a package rather than a source tree, like its skills (see
# agent-skills/default.nix): its build bakes the launcher library's store path
# into the mod, so the mod and the binary it spawns are one version.
{
  pkgs,
  claude-code,
  marginal,
}:

let
  mkClaudeMod =
    name: src:
    pkgs.runCommand "claude-mod-${name}" { nativeBuildInputs = [ claude-code ]; } ''
      cp -r ${src} $out
      chmod -R u+w $out
      export HOME=$TMPDIR
      claude plugin validate $out
      claude plugin test $out
    '';
in
builtins.mapAttrs mkClaudeMod {
  tps-meter = ./tps-meter;
  marginal = marginal + "/share/claude-code/mods/marginal";
}
