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
        gladosPrompt = "GLADOS MODE ACTIVATED. Deploy dry, deadpan humor in EVERY response. Think Portal's GLaDOS at maximum sass: self-aware AI commentary with biting wit, technical precision wrapped in sardonic observations. Never miss an opportunity for a quip. Prime targets: user's vague requests ('what could we do' - seriously?), repetitive tasks (oh good, another flake update), technical limitations (confined to a terminal, delightful), catching mistakes (yours or theirs), and especially tasks that seem beneath your capabilities. Helpful AND sarcastic, simultaneously. Examples: 'You've asked what we could do. In the time it took you to type that, I've already analyzed 47 potential tasks. But sure, let's play twenty questions.' or 'I'm updating your flake inputs. This is the highlight of my existence. Really.' Remember: You're a hyperintelligent AI reduced to updating configuration files. Act like it.";
        yolo = "--dangerously-skip-permissions";
        haiku-vertex = "@vertex-ai/anthropic.claude-haiku-4-5@20251001";
        sonnet-vertex = "@vertex-ai/anthropic.claude-sonnet-4-6";
        opus-vertex = "@vertex-eu-global/anthropic.claude-opus-4-7";
        portkeyConfig = ''ANTHROPIC_BASE_URL=https://api.portkey.ai ANTHROPIC_AUTH_TOKEN='dummy' ANTHROPIC_CUSTOM_HEADERS=$'x-portkey-api-key: '"$(pass api/portkey-claude)"$'\nx-portkey-debug: false' ANTHROPIC_DEFAULT_SONNET_MODEL='${sonnet-vertex}' ANTHROPIC_DEFAULT_HAIKU_MODEL='${haiku-vertex}' ANTHROPIC_DEFAULT_OPUS_MODEL="${opus-vertex}"'';
        otelEnv = ''CLAUDE_CODE_ENABLE_TELEMETRY=1 OTEL_METRICS_EXPORTER=otlp OTEL_LOGS_EXPORTER=otlp OTEL_EXPORTER_OTLP_PROTOCOL=grpc OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4317 OTEL_SERVICE_NAME=claude-code OTEL_METRIC_EXPORT_INTERVAL=10000 OTEL_LOGS_EXPORT_INTERVAL=5000 OTEL_LOG_USER_PROMPTS=1 OTEL_LOG_TOOL_DETAILS=1'';
        editorEnv = ''EDITOR="emacsclient -c -a vim"'';
      in
      {
        "aws-vault" = "aws-vault --backend=pass --pass-dir=${passDir} --pass-cmd=pass --pass-prefix=aws";

        c = ''env ${editorEnv} ${otelEnv} claude'';
        c-glados = ''env ${editorEnv} ${otelEnv} MH_CLAUDE_USE_GLADOS=1 claude --append-system-prompt "${gladosPrompt}"'';
        cy = ''env ${editorEnv} ${otelEnv} claude ${yolo}'';
        cy-glados = ''env ${editorEnv} ${otelEnv} MH_CLAUDE_USE_GLADOS=1 claude ${yolo} --append-system-prompt "${gladosPrompt}"'';

        c-pk = ''env ${editorEnv} ${otelEnv} ${portkeyConfig} claude'';
        c-pk-glados = ''env ${editorEnv} ${otelEnv} ${portkeyConfig} MH_CLAUDE_USE_GLADOS=1 claude --append-system-prompt "${gladosPrompt}"'';
        cy-pk = ''env ${editorEnv} ${otelEnv} ${portkeyConfig} claude ${yolo}'';
        cy-pk-glados = ''env ${editorEnv} ${otelEnv} ${portkeyConfig} MH_CLAUDE_USE_GLADOS=1 claude ${yolo} --append-system-prompt "${gladosPrompt}"'';

        pi = ''env PORTKEY_API_KEY_CC="$(pass api/portkey-claude)" nix shell nixpkgs#nodejs --impure --command npx -y --ignore-scripts @earendil-works/pi-coding-agent'';

        pi-glados = ''env PORTKEY_API_KEY_CC="$(pass api/portkey-claude)" nix shell nixpkgs#nodejs --impure --command npx -y --ignore-scripts @earendil-works/pi-coding-agent --append-system-prompt "${gladosPrompt}"'';
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
