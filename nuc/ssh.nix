userName:
{ config, pkgs, ...}:

{
  services = {
    openssh = {
      passwordAuthentication = false;
      permitRootLogin = "prohibit-password";
      extraConfig = ''
        PermitEmptyPasswords no
        AllowUsers ${userName}
      '';
    };
  };

  networking.firewall.allowedTCPPorts = [
    4225 # ssh from internet
  ];
}
