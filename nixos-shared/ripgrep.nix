{ config, pkgs, ... }:

let
  ripgreprc = pkgs.writeText "ripgreprc" ''
    --type-add
    scala:*.{scala,sbt}

    --type-add
    nix:*.nix

    --smart-case

    --hidden
  '';
in {
  environment = {
    variables = { RIPGREP_CONFIG_PATH = "${ripgreprc}"; };

    systemPackages =
      [ pkgs.ripgrep pkgs.ripgrep-all (pkgs.myScripts.ripgrepFzf) ];

    shellAliases =
      (with pkgs; { rg = "env RIPGREP_CONFIG_PATH=${ripgreprc} rg"; });
  };
}
