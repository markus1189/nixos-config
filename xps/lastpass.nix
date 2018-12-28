{ config, pkgs, ...}:

let lastpassEmail = "markus1189@gmail.com"; in
{
  environment = {
    systemPackages = [ pkgs.lastpass-cli ];

    shellAliases = {
      "lpasspw" = "${pkgs.lastpass-cli}/bin/lpass show -cp";
      "lpassi" = "${pkgs.lastpass-cli}/bin/lpass login ${lastpassEmail}";
      "lpasso" = "${pkgs.lastpass-cli}/bin/lpass logout -f";
    };
  };
}
