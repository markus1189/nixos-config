{ config, pkgs, ... }:

# Shared skeleton of the primary user account (config.my.userName).
# Hosts add their extra groups / initialPassword on top; lists merge.
{
  users.extraUsers.${config.my.userName} = {
    isNormalUser = true;
    uid = 1000;
    group = "users";
    extraGroups = [
      "wheel"
      "audio"
      "docker"
      "lp"
    ];
    shell = "${pkgs.zsh}/bin/zsh";
    home = "/home/${config.my.userName}";
  };
}
