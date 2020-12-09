{ config, pkgs, ...}:

{
  services = {
    openssh = {
      enable = true;
    };
  };

  programs = {
    ssh = {
      startAgent = true;
      extraConfig = ''
        Host p1
          HostName 192.168.178.69
          User markus

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
          Port 443
          ServerAliveInterval 60
          ServerAliveCountMax 3

        Host mctl
          HostName markus1189.no-ip.org
          User mediacenter
          DynamicForward 12345
          Port 443
          RequestTTY no

        Host mcivnc
          HostName markus1189.no-ip.org
          User mediacenter
          RequestTTY no
          LocalForward 5900 localhost:5900
          Port 443
          ServerAliveInterval 60
          ServerAliveCountMax 3

       Host nuc
          HostName 192.168.178.46
          User mediacenter
          Port 4241
          ServerAliveInterval 60
          ServerAliveCountMax 3

       Host nuci
          HostName markus1189.no-ip.org
          User mediacenter
          Port 4225
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

  users.users.${config.lib._custom_.userName}.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCyPqMvHxdzEzw+4yyBo+lqQLsvSZJ+5l+nQSVSqkc8DKwJw7inI0yggXq+P3dgGY3wfW+jk98DUP40dZDBD9wQm3HDqrRpncoCvP/4VqZG3isbOsmZsC1JJxrd83cLOn27NBvWkfiSuJE63GMM8oNKxuH0fkJov2sTOkT911nyjS9uJE/3k52X0k4LVdXSfO5gP/nZ2UsE8LQDr6/DBlK4bmMKlSJCjrwfWdBrkbMjydzV5gmEuy6bALHyN9lf2nJXZZlHbrw1cD/CkiD6OR4CYnS/YjA6VQEJTI1Ce1wuW9IkfDKHdvayW23uv7OsCs820g767hQw+ezBXerykui/ /home/markus/.ssh/id_rsa"

    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAAD9QDViXFLri6z5GhruV0wl30vMHPWBKRglolnQZZPNmPQ++MUxyKFPadU/v2XacHzSo2Del0I1Zm0EfY0PJ66jmfuFw26oWRme9qsWP1j5ubBHGVp6lD47LTq2rv2DazlOryjemdZgMbucl6dPIV9SnsQiuzLqb95Zw2+DHpqG6AXzCbCk6vUSegxTDtMn/EgWfsR0eGA6jp8aMAOaQ44aOynQMzDCcyLPaD9/IBSgZQ21cT/T6Zx50JOkSCKbMqONWHNs9ajSKot57PDslAisqzeJstci8Hkvzci8n3uFAVOw+Vqoqqs2CEEAslbFL1gKcNM5TUqZPAVRTGJ4j3F0LwXgEKt7hheSCWCnytYskryz1rBg7FMhblOdmiJ3rRrqfpNYsRRozJqmSiXzw4s9PIITTWNdlimV9oCECrVYOcNwShk2mIu/NO9Q47ipmKdnPt+iByoyJyvJN2U5rMxv/jMwwEQMHsSNq0NXJjsbO1JgiA5iQ64vTiFGTJY2DiYFFsmTJB+GWcNWmw51yXH+/lGZqKOw2LYRDC9k6snP5p7quP4eTBIn2t6yMggGfaEMHiIqwOaYhlNHcgviyYBgAkcFFEFZb834UAnMqtxyyUdT7RrnXd7pwUR7aLEcrrqYKRceyGg73H5THNXNGLhZMt1HP9JaM+EHBgUR684nW3OHOJYMNWEtZxg7ITSK8d11riT5Diy2nywBlOn35HuW3aG359i/auHVG6lLp922PF3NC14JYE615MqbNGnD4acKDpqDQlghjTJBK7SNms7RZt1mffV591kytHXl7gbwZO+P1tmY0XD7pi4dfX8PoGXlJvKaABvgUE5clPYbXeTqlO1LBLV/gvMYiAzj14aj4ratx1F4gn1FjAgcS6zC3F0EsJL5iBitGzVDz7WUl9dls7r4NMO2rKHb+tN795uTLMILlwgJBwiw0NgVyzN8akWIxW8X0hckOERAir3QKJGKGvOK1UDfyAgmbt7l23sAIThub/e9MsZecnFB1JPIlOcT7561Pk823eKefQtqxBslHrSbIxXCglwUh62F4jkuT2Fv1hEtBHMchCNnT4SzQH9bg5m0dF4S5CdH1oAvlpri5aO+kJ3/dSjPA1DwHBnU0jYzLejBtow8Qe6fHkI8joPbIw1BV1ZXXUO80Vcv3r1D7CiTed8ahDL4skm4rN2Ija+pathuGON86GtznVWVG85hxVfZ9OoBoNE77GrZ3rVn9kzKkCX/kNOQdZyYQMIeFfgJp55o1GuGwvAwVEyzX5rjJrkKjBT1bqkqAa66lwc/mE6AnFT1zjLNFx1O2rlujWHuGA2N8rldvABlAN/R9hvrLV9kLPJ markus@nixos-p1"
  ];
}
