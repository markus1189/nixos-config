{ config, pkgs, ... }:

let
  secrets = import ../nixos-shared/secrets.nix;
  mergeAttrList = pkgs.lib.foldl' pkgs.lib.mergeAttrs { };
in {
  home = {
    packages = [
      pkgs.source-code-pro
      pkgs.dunst
      pkgs.ndt
      pkgs.haskellPackages.brittany
    ];

    file = {
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

    firefox.enable = true;

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

        "double" = {
          fingerprint = {
            "DP-1" = "00ffffffffffff0010acf1a04c5234300a1e010380582578eeee95a3544c99260f5054a54b00714f81008180a940d1c00101010101012d5080a070402e6030203a00706f3100001a000000ff00354b4330333033353034524c0a000000fc0044454c4c20553338313844570a000000fd001855197322000a20202020202001f8020322f14d9005040302071601141f12135a230907078301000067030c0020003844023a801871382d40582c4500706f3100001e565e00a0a0a0295030203500706f3100001acd4600a0a0381f4030203a00706f3100001a134c00a0f040176030203a00706f3100001a000000000000000000000000000000000000000000c8";
            "DP-2" = "00ffffffffffff0010acf4a04c5234300a1e0104b55825783eee95a3544c99260f5054a54b00714f81008180a940d1c00101010101012d5080a070402e6030203a00706f3100001a000000ff00354b4330333033353034524c0a000000fc0044454c4c20553338313844570a000000fd001855197328000a202020202020016902031af14d9005040302071601141f12135a2309070783010000023a801871382d40582c4500706f3100001e565e00a0a0a0295030203500706f3100001acd4600a0a0381f4030203a00706f3100001a4c9a00a0f0402e6030203a00706f3100001a134c00a0f040176030203a00706f3100001a0000000000000000000000ea";
            "eDP-1" = "00ffffffffffff0009e5ec0800000000011d0104b523137802df50a35435b5260f50540000000101010101010101010101010101010152d000a0f0703e803020350058c21000001a00000000000000000000000000000000001a000000fe00424f452048460a202020202020000000fe004e4531353651554d2d4e36410a017702030f00e3058000e6060501737321000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008b";
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
      };
    };
  };

  services = {
    keynav.enable = true;
    flameshot.enable = true;

    dunst = (pkgs.callPackage ../nixos-shared/home-manager/dunst/default.nix
      { }).value;

  };

  fonts = { fontconfig = { enable = true; }; };
}
