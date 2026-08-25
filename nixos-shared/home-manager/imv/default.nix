{ pkgs, ... }:
let
  # imv's `exec` runs the command through system(3), i.e. /bin/sh with imv's
  # state exported as $imv_* — so the bind only has to call this.  A script
  # instead of an inline pipeline keeps the quoting out of the ini file and
  # pins xclip/notify-send by store path rather than by imv's ambient PATH.
  copyPath = pkgs.writeShellApplication {
    name = "imv-copy-path";
    runtimeInputs = with pkgs; [
      coreutils
      libnotify
      xclip
    ];
    text = ''
      file="''${imv_current_file:-}"

      if [ -z "$file" ]; then
        notify-send -a imv "imv" "no current image"
        exit 1
      fi

      # imv hands back the path exactly as it was opened, which is relative
      # whenever imv was started from the image's own directory.  Absolutise
      # real files; leave anything else (stdin's '-') alone.
      if [ -e "$file" ]; then
        file="$(realpath -- "$file")"
      fi

      # xclip -i forks a selection-owning daemon that inherits stdout/stderr;
      # redirect so it doesn't sit on imv's.
      printf '%s' "$file" | xclip -selection clipboard -i >/dev/null 2>&1

      notify-send -a imv "Copied path" "$file"
    '';
  };
in
{
  # Image viewer.  Installed here rather than in laptop/programs.nix so the
  # package and its config travel together.
  programs.imv = {
    enable = true;

    # Only [binds] — suppress_default_binds stays false, so this is additive
    # on top of imv's built-in binds.  `y` is unbound by default (vim yank).
    settings.binds."y" = "exec ${copyPath}/bin/imv-copy-path";
  };
}
