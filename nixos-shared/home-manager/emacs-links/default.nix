{ lib, pkgs, ... }:
# Under xmonad, xdg-open runs in generic mode: any MIME type without a
# default (.nix, .md, .json, .py, ...) falls through to its hardcoded browser
# list, i.e. Firefox. Ghostty opens OSC 8 links via xdg-open, so terminal file
# links landed there.
let
  # emacsclient comes from PATH: xdg-open inherits it from the terminal.
  openInEmacs = pkgs.writeShellApplication {
    name = "emacsclient-open-url";
    text = ''
      # Accepts a path or file://HOST/PATH[#FRAG]; FRAG carries the line and
      # optional column, as L12, 12, 12:3 or L12C3 (rg --hyperlink-format).
      target=$1 line="" col=""
      if [[ $target == file://* ]]; then
        target=''${target#file://}
        target=/''${target#*/}
        if [[ $target == *'#'* ]]; then
          frag=''${target#*#}
          target=''${target%%#*}
          if [[ $frag =~ ([0-9]+)(:|C)?([0-9]+)? ]]; then
            line=''${BASH_REMATCH[1]}
            col=''${BASH_REMATCH[3]}
          fi
        fi
        target=$(printf '%b' "''${target//%/\\x}")
      fi
      args=(-n -r -a "")
      [[ -n $line ]] && args+=("+$line''${col:+:$col}")
      emacsclient "''${args[@]}" "$target"
      # xmonad's activate hook (doAskUrgent) turns this into an urgency hint.
      emacsclient -n -e '(select-frame-set-input-focus (selected-frame))' >/dev/null
    '';
  };

  textTypes = [
    "text/plain"
    "text/markdown"
    "text/x-nix"
    "text/x-org"
    "text/x-log"
    "text/csv"
    "text/x-diff"
    "text/x-patch"
    "text/x-makefile"
    "text/x-shellscript"
    "application/x-shellscript"
    "text/x-python"
    "text/x-script.python"
    "text/x-haskell"
    "text/x-scala"
    "text/x-java"
    "text/x-csrc"
    "text/x-chdr"
    "text/x-c++src"
    "text/rust"
    "text/x-go"
    "text/x-lua"
    "text/javascript"
    "application/javascript"
    "application/typescript"
    "application/json"
    "application/yaml"
    "application/x-yaml"
    "application/toml"
    "application/xml"
    "text/xml"
  ];
in
{
  xdg.desktopEntries.emacsclient-open-url = {
    name = "Emacs (client)";
    exec = "${lib.getExe openInEmacs} %u";
    noDisplay = true;
    mimeType = textTypes;
  };

  xdg.mimeApps.defaultApplications = lib.genAttrs textTypes (_: [ "emacsclient-open-url.desktop" ]);
}
