{ config, pkgs, ...}:

{
  services = {
    openssh = {
      enable = true;
      passwordAuthentication = false;
      permitRootLogin = "prohibit-password";
      extraConfig = ''
        PermitEmptyPasswords no
        AllowUsers ${config.lib._custom_.userName}
      '';
      ports = [ 4241 ];
    };
  };

  programs = {
    ssh = {
      startAgent = true;
      extraConfig = ''
        Host p1
          HostName nixos-p1
          User markus
          Port 4241

        Host xps
          HostName nixos-xps
          User markus
          Port 4241

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

        Host yellow
          HostName 192.168.178.118
          User yellow

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
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCyPqMvHxdzEzw+4yyBo+lqQLsvSZJ+5l+nQSVSqkc8DKwJw7inI0yggXq+P3dgGY3wfW+jk98DUP40dZDBD9wQm3HDqrRpncoCvP/4VqZG3isbOsmZsC1JJxrd83cLOn27NBvWkfiSuJE63GMM8oNKxuH0fkJov2sTOkT911nyjS9uJE/3k52X0k4LVdXSfO5gP/nZ2UsE8LQDr6/DBlK4bmMKlSJCjrwfWdBrkbMjydzV5gmEuy6bALHyN9lf2nJXZZlHbrw1cD/CkiD6OR4CYnS/YjA6VQEJTI1Ce1wuW9IkfDKHdvayW23uv7OsCs820g767hQw+ezBXerykui/ markus@nixos-xps"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAAEAQC1Q/WdB7dsEcIUgHFEfed0lb4/NF5krDUYIbOV/x3GACo1xpWPfS01uasLxRG30notC7tcb+ps1dwZK83Fw0B+HwGA2ubJVRdmdq0VwpJda/lgqDhTlYXfOIOulBO/kJsaAjadpH2+6weieRScvCDKheVg0GLpz+7usetCN7+FW8Pt8VLGSceiGPid698kLPZsH+5jAFNH12ZuoFJ60HcmdBynQak5OtYiYHe2Lh0TWHjeDAoQkZhr0/42WpGDoni2P4vfRFPY23AD8dlsjvMLE8mtac2vuP5ZtsTSg0oWAA6jvuHfQWt8O6pTyB7PK35v/vOH10O5brACHb/6ac6Q9bF96798XxRvyvyLaRaB9EztD+OV+P1Y056WhYbq8rr/yTVTr793yFr99YtnBs8WfJ5/SPpCRnIIxwjpaFBk+omhZfH+abtRp451NX4TF7hZa8IrmV6zm6CHCqBZGkLrmq7nuRM6lTn8PNnbEmHpvOq05VignCVqopPTVobtrPYKiyw1wdH0Zy46dB3cFlGNB/NASBHf+qIV2+1OUbSAS+6Pek4fM3qPXc3BLGKAGfX2CoBWht7w/z3Yz0cFDhRXtmS0/aJQcxn8uh26m2rOlx92yh6qv/ankqEgUNwhl2LxqIY/jxyXll7i7SMYTbRl14jcU7FDx164fVnZBwoQNRwrNW1h9iwy7B30H7LlFFvZRrnWguSgrAT5CnHHb/x46Ym85t7Ve6AZ/LbBH967x4y1RvtyTJPOI8eN15Fm7XaSD7pwbkstgRerOz383X48dPT4Sy3THwjrdAF8wWjwIgMi6I/w7MfydqhpM0D95LFkYImenIbkWtt9soFhzwe3Q0FcyG/xyfClVI1xyHxm05qNqSvpCc9CEwv7EmhOFdDCG4Eb0/Sygw4qX3k6TCaIQPQVfjEwZdxgk3hAexbcdJVBWIDKOU+/TPKj5IMuSbyqpkTZG613An8NV2j3/Z25gS0rqj9d6BSmWEwykmZIkZw66Llw00ZMOIgdO+Vh7Lrq1lLxMy1NXwZu7cJbYg4mSyzdxhap1QPzEM0ILBHGsr16QSG0Pvjodn+XH0kxTPS2b3JUZyuLINwyyCaHEHyr3VYjmC6RJEhPRrE2hwixuivoulMMrDw0n/6D3NgIDxR00WNANaA1Kpmf6gipUERXUffEzYVcD7MdpRReJEab8lA9fmPqIQ3dcS7aOTt1XFuZfy9bjaZwAwgzEdEcyN9Za8aoAJS9sDQhVvqzi4G68/b74/mqV+JeqsSZwWwOSr9wPpyZRvV5lMhPWgCTPvjrtTPVe4QTnQrPMf53tUkvEA+hKJMJIhPGbk5NMBY2bOFzo39UxZcTmLuv1gDYnopv mediacenter@nuc"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAAD9QDViXFLri6z5GhruV0wl30vMHPWBKRglolnQZZPNmPQ++MUxyKFPadU/v2XacHzSo2Del0I1Zm0EfY0PJ66jmfuFw26oWRme9qsWP1j5ubBHGVp6lD47LTq2rv2DazlOryjemdZgMbucl6dPIV9SnsQiuzLqb95Zw2+DHpqG6AXzCbCk6vUSegxTDtMn/EgWfsR0eGA6jp8aMAOaQ44aOynQMzDCcyLPaD9/IBSgZQ21cT/T6Zx50JOkSCKbMqONWHNs9ajSKot57PDslAisqzeJstci8Hkvzci8n3uFAVOw+Vqoqqs2CEEAslbFL1gKcNM5TUqZPAVRTGJ4j3F0LwXgEKt7hheSCWCnytYskryz1rBg7FMhblOdmiJ3rRrqfpNYsRRozJqmSiXzw4s9PIITTWNdlimV9oCECrVYOcNwShk2mIu/NO9Q47ipmKdnPt+iByoyJyvJN2U5rMxv/jMwwEQMHsSNq0NXJjsbO1JgiA5iQ64vTiFGTJY2DiYFFsmTJB+GWcNWmw51yXH+/lGZqKOw2LYRDC9k6snP5p7quP4eTBIn2t6yMggGfaEMHiIqwOaYhlNHcgviyYBgAkcFFEFZb834UAnMqtxyyUdT7RrnXd7pwUR7aLEcrrqYKRceyGg73H5THNXNGLhZMt1HP9JaM+EHBgUR684nW3OHOJYMNWEtZxg7ITSK8d11riT5Diy2nywBlOn35HuW3aG359i/auHVG6lLp922PF3NC14JYE615MqbNGnD4acKDpqDQlghjTJBK7SNms7RZt1mffV591kytHXl7gbwZO+P1tmY0XD7pi4dfX8PoGXlJvKaABvgUE5clPYbXeTqlO1LBLV/gvMYiAzj14aj4ratx1F4gn1FjAgcS6zC3F0EsJL5iBitGzVDz7WUl9dls7r4NMO2rKHb+tN795uTLMILlwgJBwiw0NgVyzN8akWIxW8X0hckOERAir3QKJGKGvOK1UDfyAgmbt7l23sAIThub/e9MsZecnFB1JPIlOcT7561Pk823eKefQtqxBslHrSbIxXCglwUh62F4jkuT2Fv1hEtBHMchCNnT4SzQH9bg5m0dF4S5CdH1oAvlpri5aO+kJ3/dSjPA1DwHBnU0jYzLejBtow8Qe6fHkI8joPbIw1BV1ZXXUO80Vcv3r1D7CiTed8ahDL4skm4rN2Ija+pathuGON86GtznVWVG85hxVfZ9OoBoNE77GrZ3rVn9kzKkCX/kNOQdZyYQMIeFfgJp55o1GuGwvAwVEyzX5rjJrkKjBT1bqkqAa66lwc/mE6AnFT1zjLNFx1O2rlujWHuGA2N8rldvABlAN/R9hvrLV9kLPJ markus@nixos-p1"
  ];
}
