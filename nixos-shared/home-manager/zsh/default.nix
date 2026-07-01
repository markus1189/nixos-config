{ pkgs, passDir, ... }:
{
  value = {
    enable = true;
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
        # Single source of truth for the GLaDOS persona, shared with pi-agent's glados.ts
        gladosPromptFile = pkgs.writeText "glados-prompt.txt" (builtins.readFile ../../claude/glados-prompt.txt);
        gladosFlag = ''--append-system-prompt "$(cat ${gladosPromptFile})"'';
        yolo = "--dangerously-skip-permissions";
        haiku-vertex = "vertex/claude-haiku-4-5@europe-west1";
        sonnet-vertex = "vertex/claude-sonnet-5@eu";
        opus-vertex = "vertex/claude-opus-4-8@eu";
        requestyConfig = ''ANTHROPIC_BASE_URL=https://router.eu.requesty.ai ANTHROPIC_AUTH_TOKEN="$(pass api/requesty/claude-code)" ANTHROPIC_DEFAULT_SONNET_MODEL='${sonnet-vertex}' ANTHROPIC_DEFAULT_HAIKU_MODEL='${haiku-vertex}' ANTHROPIC_DEFAULT_OPUS_MODEL="${opus-vertex}"'';
        otelEnv = ''CLAUDE_CODE_ENABLE_TELEMETRY=1 OTEL_METRICS_EXPORTER=otlp OTEL_LOGS_EXPORTER=otlp OTEL_EXPORTER_OTLP_PROTOCOL=grpc OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4317 OTEL_SERVICE_NAME=claude-code OTEL_METRIC_EXPORT_INTERVAL=10000 OTEL_LOGS_EXPORT_INTERVAL=5000 OTEL_LOG_USER_PROMPTS=1 OTEL_LOG_TOOL_DETAILS=1'';
        editorEnv = ''EDITOR="emacsclient -c -a vim"'';
      in
      {
        "aws-vault" = "aws-vault --backend=pass --pass-dir=${passDir} --pass-cmd=pass --pass-prefix=aws";

        c = ''env ${editorEnv} ${otelEnv} claude'';
        c-glados = ''env ${editorEnv} ${otelEnv} MH_CLAUDE_USE_GLADOS=1 claude ${gladosFlag}'';
        cy = ''env ${editorEnv} ${otelEnv} claude ${yolo}'';
        cy-glados = ''env ${editorEnv} ${otelEnv} MH_CLAUDE_USE_GLADOS=1 claude ${yolo} ${gladosFlag}'';

        c-rq = ''env ${editorEnv} ${otelEnv} ${requestyConfig} claude'';
        c-rq-glados = ''env ${editorEnv} ${otelEnv} ${requestyConfig} MH_CLAUDE_USE_GLADOS=1 claude ${gladosFlag}'';
        cy-rq = ''env ${editorEnv} ${otelEnv} ${requestyConfig} claude ${yolo}'';
        cy-rq-glados = ''env ${editorEnv} ${otelEnv} ${requestyConfig} MH_CLAUDE_USE_GLADOS=1 claude ${yolo} ${gladosFlag}'';

        pi = ''env REQUESTY_API_KEY_CC="$(pass api/requesty/agent)" nix shell nixpkgs#nodejs --impure --command npx -y --ignore-scripts @earendil-works/pi-coding-agent'';

        pi-glados = ''env REQUESTY_API_KEY_CC="$(pass api/requesty/agent)" nix shell nixpkgs#nodejs --impure --command npx -y --ignore-scripts @earendil-works/pi-coding-agent ${gladosFlag}'';
      };

    initContent = ''
      source ${pkgs.ndtSources.zsh-histdb}/sqlite-history.zsh
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
