{ config, pkgs, ...}:

let
  ripgreprc = pkgs.writeText "ripgreprc" ''
    --type-add
    scala:*.{scala,sbt}*

    --smart-case
  '';
in
{
  environment = {
    variables = {
      RIPGREP_CONFIG_PATH = "${ripgreprc}";
    };

    systemPackages = [
      pkgs.ripgrep
    ];

    shellAliases = (with pkgs; {
      rg = "env RIPGREP_CONFIG_PATH=${ripgreprc} rg";
    });
  };
}