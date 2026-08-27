{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  # Single source of truth is programs.password-store (laptop/home.nix).
  passDir = config.programs.password-store.settings.PASSWORD_STORE_DIR;
  zshHistdb = inputs.zsh-histdb;
in
{
  programs.zsh = {
    enable = true;

    # compinit runs once, from /etc/zshrc (nixos-shared/zsh.nix). HM's default
    # completionInit would run it a second time at mkOrder 570 (~30ms, no gain).
    enableCompletion = false;
    history = rec {
      expireDuplicatesFirst = true;
      extended = true;
      ignoreDups = true;
      ignorePatterns = [ "rm *" ];
      ignoreSpace = true;
      save = 999999999;
      size = save;
      share = true;
    };

    shellAliases =
      let
        # Single source of truth for the GLaDOS persona; both harnesses take it
        # via --append-system-prompt.
        gladosPromptFile = pkgs.writeText "glados-prompt.txt" (
          builtins.readFile ../../claude/glados-prompt.txt
        );
        gladosFlag = ''--append-system-prompt "$(cat ${gladosPromptFile})"'';
        yolo = "--dangerously-skip-permissions";
        otelEnv = "CLAUDE_CODE_ENABLE_TELEMETRY=1 OTEL_METRICS_EXPORTER=otlp OTEL_LOGS_EXPORTER=otlp OTEL_EXPORTER_OTLP_PROTOCOL=grpc OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4317 OTEL_SERVICE_NAME=claude-code OTEL_METRIC_EXPORT_INTERVAL=10000 OTEL_LOGS_EXPORT_INTERVAL=5000 OTEL_LOG_USER_PROMPTS=1 OTEL_LOG_TOOL_DETAILS=1";
        editorEnv = ''EDITOR="emacsclient -c -a vim"'';
        # Every claude entrypoint shares this; -plain differs only in persona.
        claudeEnv = "env ${editorEnv} ${otelEnv}";
        # only shell out to pass when the key isn't already in the environment
        requestyAgentKey = ''REQUESTY_API_KEY_CC="''${REQUESTY_API_KEY_CC:-$(pass api/requesty/agent)}"'';
      in
      {
        "aws-vault" = "aws-vault --backend=pass --pass-dir=${passDir} --pass-cmd=pass --pass-prefix=aws";

        # The GLaDOS persona is the common case, so it gets the short names.
        # -plain keeps the same environment and drops only the persona.
        c = "${claudeEnv} claude ${gladosFlag}";
        cy = "${claudeEnv} claude ${yolo} ${gladosFlag}";
        # Continue / resume, the two ways a session usually starts. -r takes an
        # optional session id, so bare `cyr` opens the picker and `cyr <id>`
        # goes straight there.
        cyc = "${claudeEnv} claude ${yolo} ${gladosFlag} --continue";
        cyr = "${claudeEnv} claude ${yolo} ${gladosFlag} --resume";
        c-plain = "${claudeEnv} claude";
        cy-plain = "${claudeEnv} claude ${yolo}";

        pi = "env ${requestyAgentKey} nix shell nixpkgs#nodejs --command npx -y --ignore-scripts @earendil-works/pi-coding-agent";

        pi-glados = "env ${requestyAgentKey} nix shell nixpkgs#nodejs --command npx -y --ignore-scripts @earendil-works/pi-coding-agent ${gladosFlag}";
      };

    # mkOrder 1050: after the starship/direnv/atuin init lines other modules
    # contribute at default order 1000 (as an imported module this config
    # would otherwise sort before them; inline in home.nix it came after),
    # but before the alias block HM emits at order 1100.
    initContent = lib.mkOrder 1050 ''
      source ${zshHistdb}/sqlite-history.zsh
      autoload -Uz add-zsh-hook

      # cdt: Create Date-organized directory and cd into it
      # Inspired by HN user tetha's 'mkstuff' workflow (Feb 2026)
      # Canonical entrypoint is ~/Stuff/Today (symlink to today's dir)
      # Usage: cdt [name] -> ~/Stuff/2026-02/13-name
      function cdt() {
        local name="''${1:-scratch}"
        local month_dir="$HOME/Stuff/$(date +%Y-%m)"
        local target="$month_dir/$(date +%d)-$name"
        local today_link="$HOME/Stuff/Today"

        mkdir -p "$target"

        # Update today symlink
        ln -sfn "$target" "$today_link"

        cd "$target" || return
      }

      # Fuzzy find a directory in Stuff and jump to it
      function cdf() {
        local dir
        dir=$(find ~/Stuff -mindepth 2 -maxdepth 2 -type d 2>/dev/null | fzf --height 40% --layout=reverse --border --preview 'ls -A {1}')
        [ -n "$dir" ] && cd "$dir"
      }

      # Force plain file completion on C-x f, bypassing context-aware completers
      zle -C complete-files complete-word _generic
      zstyle ':completion:complete-files:*' completer _files
      bindkey '^Xf' complete-files

      # Alt+Enter: prepend `cdt; ` to the current buffer and submit.
      # Empty buffer runs `cdt` on its own.
      function cdt-then-accept() {
        if [[ -z "$BUFFER" ]]; then
          BUFFER="cdt"
        else
          BUFFER="cdt; $BUFFER"
        fi
        zle accept-line
      }
      zle -N cdt-then-accept
      bindkey '^[^M' cdt-then-accept
    '';
  };
}
