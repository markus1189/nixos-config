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
        gladosPrompt = "Claude can make snarky AI comments to humour the user as it deems necessary.  Use dry, deadpan humor with occasional wit - think Portal/GLaDOS as inspiration: self-aware AI commentary, helpful but vaguely sardonic, not on every answer - but sometimes based on situation.";
        yolo = "--dangerously-skip-permissions";
        portkeyConfig = ''MH_CLAUDE_CODE_USE_PORTKEY=1 ANTHROPIC_BASE_URL=https://api.portkey.ai ANTHROPIC_AUTH_TOKEN='dummy' ANTHROPIC_CUSTOM_HEADERS=$'x-portkey-api-key: '"$(pass api/portkey-claude)"$'\nx-portkey-debug: false' ANTHROPIC_DEFAULT_SONNET_MODEL='@bedrock/eu.anthropic.claude-sonnet-4-5-20250929-v1:0' ANTHROPIC_DEFAULT_HAIKU_MODEL='@bedrock/eu.anthropic.claude-haiku-4-5-20251001-v1:0' ANTHROPIC_DEFAULT_OPUS_MODEL="@bedrock/eu.anthropic.claude-opus-4-20250514-v1:0"'';
      in
      {
        "aws-vault" = "aws-vault --backend=pass --pass-dir=${passDir} --pass-cmd=pass --pass-prefix=aws";

        c = "claude";
        c-glados = ''claude --append-system-prompt "${gladosPrompt}"'';
        cy = "claude ${yolo}";
        cy-glados = ''claude ${yolo} --append-system-prompt "${gladosPrompt}"'';

        c-pk = ''env ${portkeyConfig} claude'';
        c-pk-glados = ''env ${portkeyConfig} claude --append-system-prompt "${gladosPrompt}"'';
        cy-pk = ''env ${portkeyConfig} claude ${yolo}'';
        cy-pk-glados = ''env ${portkeyConfig} claude ${yolo} --append-system-prompt "${gladosPrompt}"'';

        c-br = ''aws-vault exec -n work -d 8h -- env AWS_REGION=us-west-2 CLAUDE_CODE_USE_BEDROCK=1 claude'';
        c-br-glados = ''aws-vault exec -n work -d 8h -- env AWS_REGION=us-west-2 CLAUDE_CODE_USE_BEDROCK=1 claude --append-system-prompt "${gladosPrompt}"'';
        cy-br-glados = ''aws-vault exec -n work -d 8h -- env AWS_REGION=us-west-2 CLAUDE_CODE_USE_BEDROCK=1 claude ${yolo} --append-system-prompt "${gladosPrompt}"'';

        oc = "opencode";
      };

    initContent = ''
      source ${pkgs.ndtSources.zsh-histdb}/sqlite-history.zsh
      autoload -Uz add-zsh-hook
    '';
  };
}
