{ config, pkgs, ...}:

{
  services = {
    openssh.enable = true;
  };

  programs = {
    ssh = {
      startAgent = true;
      extraConfig = ''
        Host yellow
          HostName 192.168.178.118
          User yellow

        Host mc
          HostName 192.168.178.86
          User mediacenter
          ServerAliveInterval 60
          ServerAliveCountMax 3

        Host mci
          HostName markus1189.no-ip.org
          User mediacenter
          Port 4224
          ServerAliveInterval 60
          ServerAliveCountMax 3

        Host mctl
          HostName markus1189.no-ip.org
          User mediacenter
          DynamicForward 12345
          Port 4224
          RequestTTY no

        Host mcivnc
          HostName markus1189.no-ip.org
          User mediacenter
          RequestTTY no
          LocalForward 5900 localhost:5900
          Port 4224
          ServerAliveInterval 60
          ServerAliveCountMax 3

        Host pi
          HostName 192.168.178.60
          User pi
      '';
    };
  };

  environment = {
    interactiveShellInit = ''
      ssht() {
        ssh -t $1 'tmux attach'
      }
    '';
  };
}
