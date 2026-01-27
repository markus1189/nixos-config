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
        haiku-br = "@bedrock/eu.anthropic.claude-haiku-4-5-20251001-v1:0";
        sonnet-br = "@bedrock/eu.anthropic.claude-sonnet-4-5-20250929-v1:0";
        opus-br = "@bedrock/eu.anthropic.claude-opus-4-5-20251101-v1:0";
        haiku-vertex = "@vertex-ai/anthropic.claude-haiku-4-5@20251001";
        sonnet-vertex = "@vertex-ai/anthropic.claude-sonnet-4-5@20250929";
        opus-vertex = "@vertex-ai/anthropic.claude-opus-4-5@20251101";
        portkeyConfig = ''ANTHROPIC_BASE_URL=https://api.portkey.ai ANTHROPIC_AUTH_TOKEN='dummy' ANTHROPIC_CUSTOM_HEADERS=$'x-portkey-api-key: '"$(pass api/portkey-claude)"$'\nx-portkey-debug: false' ANTHROPIC_DEFAULT_SONNET_MODEL='${sonnet-vertex}' ANTHROPIC_DEFAULT_HAIKU_MODEL='${haiku-vertex}' ANTHROPIC_DEFAULT_OPUS_MODEL="${opus-vertex}"'';
      in
      {
        "aws-vault" = "aws-vault --backend=pass --pass-dir=${passDir} --pass-cmd=pass --pass-prefix=aws";

        c = "claude";
        c-glados = ''env MH_CLAUDE_USE_GLADOS=1 claude --append-system-prompt "${gladosPrompt}"'';
        cy = "claude ${yolo}";
        cy-glados = ''env MH_CLAUDE_USE_GLADOS=1 claude ${yolo} --append-system-prompt "${gladosPrompt}"'';

        c-pk = ''env ${portkeyConfig} claude'';
        c-pk-glados = ''env ${portkeyConfig} MH_CLAUDE_USE_GLADOS=1 claude --append-system-prompt "${gladosPrompt}"'';
        cy-pk = ''env ${portkeyConfig} claude ${yolo}'';
        cy-pk-glados = ''env ${portkeyConfig} MH_CLAUDE_USE_GLADOS=1 claude ${yolo} --append-system-prompt "${gladosPrompt}"'';

        c-br = ''aws-vault exec -n work -d 8h -- env AWS_REGION=us-west-2 CLAUDE_CODE_USE_BEDROCK=1 claude'';
        c-br-glados = ''aws-vault exec -n work -d 8h -- env AWS_REGION=us-west-2 CLAUDE_CODE_USE_BEDROCK=1 MH_CLAUDE_USE_GLADOS=1 claude --append-system-prompt "${gladosPrompt}"'';
        cy-br-glados = ''aws-vault exec -n work -d 8h -- env AWS_REGION=us-west-2 CLAUDE_CODE_USE_BEDROCK=1 MH_CLAUDE_USE_GLADOS=1 claude ${yolo} --append-system-prompt "${gladosPrompt}"'';

        oc = "opencode";

        pi = ''env PORTKEY_API_KEY_CC="$(pass api/portkey-claude)" nix shell nixpkgs#nodejs --impure --command npx -y @mariozechner/pi-coding-agent'';

        pi-glados = ''env PORTKEY_API_KEY_CC="$(pass api/portkey-claude)" nix shell nixpkgs#nodejs --impure --command npx -y @mariozechner/pi-coding-agent --append-system-prompt "${gladosPrompt}"'';
      };

    initContent = ''
      source ${pkgs.ndtSources.zsh-histdb}/sqlite-history.zsh
      autoload -Uz add-zsh-hook
    '';
  };
}
