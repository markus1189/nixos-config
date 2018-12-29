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
      ports = [ 4241 ];
    };
  };
}
