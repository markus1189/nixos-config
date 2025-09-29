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

    shellAliases = {
      "aws-vault" = "aws-vault --backend=pass --pass-dir=${passDir} --pass-cmd=pass --pass-prefix=aws";
      "c" = "claude";
      "cy" = "claude --dangerously-skip-permissions";
      "claude-yolo" = "claude --dangerously-skip-permissions";
    };

    initContent = ''
      source ${pkgs.ndtSources.zsh-histdb}/sqlite-history.zsh
      autoload -Uz add-zsh-hook
    '';
  };
}
