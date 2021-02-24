{ config, pkgs, ... }:

let
  secrets = import ../nixos-shared/secrets.nix;
  mergeAttrList = pkgs.lib.foldl' pkgs.lib.mergeAttrs { };
in {
  home = {
    packages = with pkgs; [
      source-code-pro
      dunst
      ndt
      haskellPackages.brittany
      jrnl
    ];

    file = {
      "flameshot-config" = {
        target = ".config/flameshot/flameshot.ini";
        text = ''
          [General]
          disabledTrayIcon=false
          drawColor=#ff0000
          drawThickness=0
          saveAfterCopyPath=/home/markus/Downloads
          savePath=/home/markus/Downloads
          uiColor=#ee8903
        '';
      };
      "edbrowse-config" = {
        target = ".ebrc";
        text = ''
          #  The cookie jar-where we store the http cookies.
          #jar=/home/mylogin/outside/cookies

          webtimer=60
          mailtimer=180

          agent=Lynx/2.8.4rel.1 libwww-FM/2.14
          agent=Mozilla/4.0(compatible;MSIE 7.0;Windows NT 6.1;WOW64;Trident/5.0;SLCC2; .NET CLR 2.0.50727; .NET CLR 3.5.30729; .NET CLR 3.0.30729;Media Center PC 6.0; .NET4.0C; .NET4.0E)

          function:init{
          # Display the size of each file in a directory listing
          ls=s

          # character limit when displaying a line
          ll 700
          # enable readline
          rl+
          }
        '';
      };
      "gtk-bookmarks" = {
        text = ''
          file:///home/markus/Downloads
          file:///home/markus/Dropbox
          file:///home/markus/repos
          file:///home/markus/Photos/web
          file:///home/markus/Photos/developed
          file:///home/markus/repos/nixos-config
        '';
        target = ".gtk-bookmarks";
      };
      "keynavrc" = {
        source = pkgs.callPackage ../nixos-shared/home-manager/keynav { };
        target = ".keynavrc";
      };

      "mrconfig" = {
        target = ".mrconfig";
        text = ''
          [repos/clones/nixpkgs]
          checkout = git clone 'https://github.com/NixOS/nixpkgs' 'nixpkgs'

          [repos/nixos-config]
          checkout = git clone 'git@github.com:markus1189/nixos-config.git' 'nixos-config'
        '';
      };

      "jrnl_config" = {
        target = ".jrnl_config";
        text = builtins.toJSON {
          journals = { default = "/home/markus/Dropbox/journal.txt"; };
          editor = "${pkgs.vim}/bin/vim";
          encrypt = false;
          default_hour = 9;
          default_minute = 0;
          timeformat = "%Y-%m-%dT%H:%M:%S";
          tagsymbols = "@";
          highlight = true;
          linewrap = 79;
        };
      };

      "mpv_config" = {
        target = ".config/mpv/mpv.conf";
        text = ''
          save-position-on-quit=yes
          osd-msg3="''${time-pos} / ''${duration} [''${playtime-remaining} @ ''${speed}]"
          osd-duration=5000
        '';
      };

      "mpv_input_conf" = {
        target = ".config/mpv/input.conf";
        text = ''
          ! run sh -c "echo ''${path} >> list.txt && notify-send mpv \"Saved ''${path} to ''${working-directory}/list.txt\""
        '';
      };

      "global-sbt-aliases" = {
        target = ".sbt/1.0/global-aliases.sbt";
        text = ''
          addCommandAlias("r", "reload")
          addCommandAlias("c", "compile")
          addCommandAlias("t", "test")
          addCommandAlias("to", "testOnly")
        '';
      };
    };
  };

  manual = {
    html.enable = true;
    json.enable = true;
    manpages.enable = true;
  };

  programs = {
    bash.enable = true;
    zsh.enable = true;
    direnv = {
      enable = true;
      enableBashIntegration = true;
      enableNixDirenvIntegration = true;
      enableZshIntegration = true;
    };

    firefox = (pkgs.callPackage ../nixos-shared/home-manager/firefox/default.nix
      { }).value;

    git =
      (pkgs.callPackage ../nixos-shared/home-manager/git/default.nix { }).value;

    vim =
      (pkgs.callPackage ../nixos-shared/home-manager/vim/default.nix { }).value;

    mpv = { enable = true; };

    autorandr = {
      enable = true;
      profiles = {
        "mobile" = {
          fingerprint = {
            "eDP-1" =
              "00ffffffffffff0009e5ec0800000000011d0104b523137802df50a35435b5260f50540000000101010101010101010101010101010152d000a0f0703e803020350058c21000001a00000000000000000000000000000000001a000000fe00424f452048460a202020202020000000fe004e4531353651554d2d4e36410a017702030f00e3058000e6060501737321000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008b";
          };
          config = {
            "DP-1".enable = false;
            "DP-2".enable = false;
            "DP-3".enable = false;
            "eDP-1" = {
              enable = true;
              crtc = 0;
              primary = true;
              position = "0x0";
              mode = " 1920x1080";
              rate = "60.00";
            };
          };
        };

        "half" = {
          fingerprint = {
            "DP-1" =
              "00ffffffffffff0010acf1a04c5234300a1e010380582578eeee95a3544c99260f5054a54b00714f81008180a940d1c00101010101012d5080a070402e6030203a00706f3100001a000000ff00354b4330333033353034524c0a000000fc0044454c4c20553338313844570a000000fd001855197322000a20202020202001f8020322f14d9005040302071601141f12135a230907078301000067030c0020003844023a801871382d40582c4500706f3100001e565e00a0a0a0295030203500706f3100001acd4600a0a0381f4030203a00706f3100001a134c00a0f040176030203a00706f3100001a000000000000000000000000000000000000000000c8";
            "eDP-1" =
              "00ffffffffffff0009e5ec0800000000011d0104b523137802df50a35435b5260f50540000000101010101010101010101010101010152d000a0f0703e803020350058c21000001a00000000000000000000000000000000001a000000fe00424f452048460a202020202020000000fe004e4531353651554d2d4e36410a017702030f00e3058000e6060501737321000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008b";
          };

          config = {
            "DP-1" = {
              enable = true;
              primary = true;
              position = "0x0";
              mode = "1920x1600";
              rate = "60.00";
            };
            "DP-2".enable = false;
            "DP-3".enable = false;
            "eDP-1" = {
              enable = true;
              position = "1920x0";
              mode = "1920x1080";
              rate = "60.00";
            };
          };
        };

        "half-with-dongle-2" = {
          fingerprint = {
            "DP-3" =
              "00ffffffffffff0010acf1a04c5234300a1e010380582578eeee95a3544c99260f5054a54b00714f81008180a940d1c00101010101012d5080a070402e6030203a00706f3100001a000000ff00354b4330333033353034524c0a000000fc0044454c4c20553338313844570a000000fd001855197322000a20202020202001f8020322f14d9005040302071601141f12135a230907078301000067030c0020003844023a801871382d40582c4500706f3100001e565e00a0a0a0295030203500706f3100001acd4600a0a0381f4030203a00706f3100001a134c00a0f040176030203a00706f3100001a000000000000000000000000000000000000000000c8";
            "eDP-1" =
              "00ffffffffffff0009e5ec0800000000011d0104b523137802df50a35435b5260f50540000000101010101010101010101010101010152d000a0f0703e803020350058c21000001a00000000000000000000000000000000001a000000fe00424f452048460a202020202020000000fe004e4531353651554d2d4e36410a017702030f00e3058000e6060501737321000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008b";
          };

          config = {
            "DP-1".enable = false;
            "DP-2".enable = false;
            "DP-3" = {
              enable = true;
              primary = true;
              position = "0x0";
              mode = "1920x1600";
              rate = "60.00";
            };
            "eDP-1" = {
              enable = true;
              position = "1920x0";
              mode = "1920x1080";
              rate = "60.00";
            };
          };
        };

        "half-with-dongle-3" = {
          fingerprint = {
            "DP-2" =
              "00ffffffffffff0010acf1a04c5234300a1e010380582578eeee95a3544c99260f5054a54b00714f81008180a940d1c00101010101012d5080a070402e6030203a00706f3100001a000000ff00354b4330333033353034524c0a000000fc0044454c4c20553338313844570a000000fd001855197322000a20202020202001f8020322f14d9005040302071601141f12135a230907078301000067030c0020003844023a801871382d40582c4500706f3100001e565e00a0a0a0295030203500706f3100001acd4600a0a0381f4030203a00706f3100001a134c00a0f040176030203a00706f3100001a000000000000000000000000000000000000000000c8";
            "eDP-1" =
              "00ffffffffffff0009e5ec0800000000011d0104b523137802df50a35435b5260f50540000000101010101010101010101010101010152d000a0f0703e803020350058c21000001a00000000000000000000000000000000001a000000fe00424f452048460a202020202020000000fe004e4531353651554d2d4e36410a017702030f00e3058000e6060501737321000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008b";
          };

          config = {
            "DP-1".enable = false;
            "DP-2" = {
              enable = true;
              primary = true;
              position = "0x0";
              mode = "1920x1600";
              rate = "60.00";
            };
            "DP-3".enable = false;
            "eDP-1" = {
              enable = true;
              position = "1920x0";
              mode = "1920x1080";
              rate = "60.00";
            };
          };
        };

        "double" = {
          fingerprint = {
            "DP-1" =
              "00ffffffffffff0010acf1a04c5234300a1e010380582578eeee95a3544c99260f5054a54b00714f81008180a940d1c00101010101012d5080a070402e6030203a00706f3100001a000000ff00354b4330333033353034524c0a000000fc0044454c4c20553338313844570a000000fd001855197322000a20202020202001f8020322f14d9005040302071601141f12135a230907078301000067030c0020003844023a801871382d40582c4500706f3100001e565e00a0a0a0295030203500706f3100001acd4600a0a0381f4030203a00706f3100001a134c00a0f040176030203a00706f3100001a000000000000000000000000000000000000000000c8";
            "DP-2" =
              "00ffffffffffff0010acf4a04c5234300a1e0104b55825783eee95a3544c99260f5054a54b00714f81008180a940d1c00101010101012d5080a070402e6030203a00706f3100001a000000ff00354b4330333033353034524c0a000000fc0044454c4c20553338313844570a000000fd001855197328000a202020202020016902031af14d9005040302071601141f12135a2309070783010000023a801871382d40582c4500706f3100001e565e00a0a0a0295030203500706f3100001acd4600a0a0381f4030203a00706f3100001a4c9a00a0f0402e6030203a00706f3100001a134c00a0f040176030203a00706f3100001a0000000000000000000000ea";
            "eDP-1" =
              "00ffffffffffff0009e5ec0800000000011d0104b523137802df50a35435b5260f50540000000101010101010101010101010101010152d000a0f0703e803020350058c21000001a00000000000000000000000000000000001a000000fe00424f452048460a202020202020000000fe004e4531353651554d2d4e36410a017702030f00e3058000e6060501737321000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008b";
          };

          config = {
            "DP-1" = {
              enable = true;
              position = "1920x0";
              mode = "1920x1600";
              rate = "60.00";
            };
            "DP-2" = {
              enable = true;
              position = "0x0";
              mode = "1920x1600";
              rate = "60.00";
            };
            "DP-3".enable = false;
            "eDP-1".enable = false;
          };
        };

        "double2" = {
          fingerprint = {
            "DP-1" =
              "00ffffffffffff0010acf1a04c5234300a1e010380582578eeee95a3544c99260f5054a54b00714f81008180a940d1c00101010101012d5080a070402e6030203a00706f3100001a000000ff00354b4330333033353034524c0a000000fc0044454c4c20553338313844570a000000fd001855197322000a20202020202001f8020322f14d9005040302071601141f12135a230907078301000067030c0020003844023a801871382d40582c4500706f3100001e565e00a0a0a0295030203500706f3100001acd4600a0a0381f4030203a00706f3100001a134c00a0f040176030203a00706f3100001a000000000000000000000000000000000000000000c8";
            "DP-3" =
              "00ffffffffffff0010acf4a04c5234300a1e0104b55825783eee95a3544c99260f5054a54b00714f81008180a940d1c00101010101012d5080a070402e6030203a00706f3100001a000000ff00354b4330333033353034524c0a000000fc0044454c4c20553338313844570a000000fd001855197328000a202020202020016902031af14d9005040302071601141f12135a2309070783010000023a801871382d40582c4500706f3100001e565e00a0a0a0295030203500706f3100001acd4600a0a0381f4030203a00706f3100001a4c9a00a0f0402e6030203a00706f3100001a134c00a0f040176030203a00706f3100001a0000000000000000000000ea";
            "eDP-1" =
              "00ffffffffffff0009e5ec0800000000011d0104b523137802df50a35435b5260f50540000000101010101010101010101010101010152d000a0f0703e803020350058c21000001a00000000000000000000000000000000001a000000fe00424f452048460a202020202020000000fe004e4531353651554d2d4e36410a017702030f00e3058000e6060501737321000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008b";
          };

          config = {
            "DP-1" = {
              enable = true;
              position = "1920x0";
              mode = "1920x1600";
              rate = "60.00";
            };
            "DP-2".enable = false;
            "DP-3" = {
              enable = true;
              position = "0x0";
              mode = "1920x1600";
              rate = "60.00";
            };
            "eDP-1".enable = false;
          };
        };

        "double3" = {
          fingerprint = {
            "DP-2" =
              "00ffffffffffff0010acf1a04c5234300a1e010380582578eeee95a3544c99260f5054a54b00714f81008180a940d1c00101010101012d5080a070402e6030203a00706f3100001a000000ff00354b4330333033353034524c0a000000fc0044454c4c20553338313844570a000000fd001855197322000a20202020202001f8020322f14d9005040302071601141f12135a230907078301000067030c0020003844023a801871382d40582c4500706f3100001e565e00a0a0a0295030203500706f3100001acd4600a0a0381f4030203a00706f3100001a134c00a0f040176030203a00706f3100001a000000000000000000000000000000000000000000c8";
            "DP-3" =
              "00ffffffffffff0010acf4a04c5234300a1e0104b55825783eee95a3544c99260f5054a54b00714f81008180a940d1c00101010101012d5080a070402e6030203a00706f3100001a000000ff00354b4330333033353034524c0a000000fc0044454c4c20553338313844570a000000fd001855197328000a202020202020016902031af14d9005040302071601141f12135a2309070783010000023a801871382d40582c4500706f3100001e565e00a0a0a0295030203500706f3100001acd4600a0a0381f4030203a00706f3100001a4c9a00a0f0402e6030203a00706f3100001a134c00a0f040176030203a00706f3100001a0000000000000000000000ea";
            "eDP-1" =
              "00ffffffffffff0009e5ec0800000000011d0104b523137802df50a35435b5260f50540000000101010101010101010101010101010152d000a0f0703e803020350058c21000001a00000000000000000000000000000000001a000000fe00424f452048460a202020202020000000fe004e4531353651554d2d4e36410a017702030f00e3058000e6060501737321000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008b";
          };

          config = {
            "DP-1".enable = false;
            "DP-2" = {
              enable = true;
              position = "1920x0";
              mode = "1920x1600";
              rate = "60.00";
            };
            "DP-3" = {
              enable = true;
              position = "0x0";
              mode = "1920x1600";
              rate = "60.00";
            };
            "eDP-1".enable = false;
          };
        };
      };
    };
  };

  services = {
    flameshot.enable = true;

    dunst = (pkgs.callPackage ../nixos-shared/home-manager/dunst/default.nix
      { }).value;

  };

  fonts = { fontconfig = { enable = true; }; };

  systemd.user.services.arbtt = let
    arbttPackage = with pkgs.haskell.lib;
      overrideSrc (doJailbreak (pkgs.haskellPackages.arbtt.overrideAttrs
        (old: rec { meta.broken = false; }))) {
          src = pkgs.fetchFromGitHub {
            owner = "nomeata";
            repo = "arbtt";
            rev = "f4b192b22e7ab7f1b3ebd5fefe51b012754f0ef8";
            sha256 = "1l1f36m5xnp07xgjjnr1v13zsbdlgfmsiv6c95jchgw1zh57gkcf";
          };
        };
  in {
    Unit = { Description = "arbtt statistics capture service"; };

    Install = { WantedBy = [ "graphical-session.target" ]; };

    Service = {
      Type = "simple";
      ExecStart =
        "${arbttPackage}/bin/arbtt-capture --logfile=%h/.arbtt/capture.log --sample-rate=${
          toString 60
        }";
      Restart = "always";
    };
  };
}
