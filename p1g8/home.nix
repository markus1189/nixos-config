# Autorandr profiles for p1g8 (Arrow Lake, modesetting driver): the
# external DisplayPorts enumerate as DP-4/DP-5 and the internal panel is
# an AUO B160UAN06.N (16", 1920x1200) — different EDID and connector
# names from the old nixos-p1, hence a per-host profile set (merged into
# the shared laptop/home.nix config via home-manager.users.*.imports).
{ pkgs, ... }:

{
  programs.autorandr.profiles =
    let
      internalDisplayG8 = "00ffffffffffff0006afb42bb42b000015220104a5221678033431a9544a9f230d5257000000010101010101010101010101010101018c3c80a070b028403020a50058d710000018000000fd00283c4b4b10010a202020202020000000fe0041554f0a202020202020202020000000fc004231363055414e30362e4e200a016a7020790200220014775d02857f079f002f801f00af04270009000400250109775d02775d02283c802b000c2700283b00002700283b0000810015741a00000301283c0000000000003c000000008d0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009b90";
      secondHalf = "00ffffffffffff0010acf4a04c5234300a1e0104b55825783eee95a3544c99260f5054a54b00714f81008180a940d1c00101010101012d5080a070402e6030203a00706f3100001a000000ff00354b4330333033353034524c0a000000fc0044454c4c20553338313844570a000000fd001855197328000a202020202020016902031af14d9005040302071601141f12135a2309070783010000023a801871382d40582c4500706f3100001e565e00a0a0a0295030203500706f3100001acd4600a0a0381f4030203a00706f3100001a4c9a00a0f0402e6030203a00706f3100001a134c00a0f040176030203a00706f3100001a0000000000000000000000ea";
    in
    {
      "mobile" = {
        fingerprint = {
          "eDP-1" = internalDisplayG8;
        };
        config = {
          "DP-4".enable = false;
          "DP-5".enable = false;
          "eDP-1" = {
            enable = true;
            crtc = 0;
            primary = true;
            position = "0x0";
            mode = "1920x1200";
            rate = "60.10";
          };
        };
        hooks.postswitch = ''
          ${pkgs.feh}/bin/feh --no-fehbg --bg-fill ${pkgs.markus-wallpapers.orange-cube-internal}
        '';
      };

      # Single Dell U3818DW ultrawide in MST mode: two 1920x1600 tiles
      # (DP-4 = left/primary, DP-5 = right) reconstructing 3840x1600.
      # Same physical monitor as the nixos-p1 homeoffice profile — only
      # the connector names changed (DP-3/DP-2 -> DP-4/DP-5).
      "homeoffice" = {
        fingerprint = {
          "DP-4" = secondHalf;
          "DP-5" =
            "00ffffffffffff0010acefa04c5234300a1e010380582578eeee95a3544c99260f5054a54b00714f81008180a940d1c00101010101012d5080a070402e6030203a00706f3100001a000000ff00354b4330333033353034524c0a000000fc0044454c4c20553338313844570a000000fd001855197322000a20202020202001fa020322f14d9005040302071601141f12135a230907078301000067030c0010003844023a801871382d40582c4500706f3100001e565e00a0a0a0295030203500706f3100001acd4600a0a0381f4030203a00706f3100001a134c00a0f040176030203a00706f3100001a000000000000000000000000000000000000000000d8";
          "eDP-1" = internalDisplayG8;
        };
        config = {
          "DP-4" = {
            primary = true;
            enable = true;
            crtc = 0;
            position = "0x0";
            mode = "1920x1600";
            rate = "59.95";
          };
          "DP-5" = {
            enable = true;
            crtc = 2;
            position = "1920x0";
            mode = "1920x1600";
            rate = "59.95";
          };
          "eDP-1".enable = false;
        };
        hooks.postswitch = ''
          ${pkgs.feh}/bin/feh --no-fehbg --bg-fill ${pkgs.markus-wallpapers.orange-cube-left} ${pkgs.markus-wallpapers.orange-cube-right}
        '';
      };

      # Zwift: laptop at the desk driving the living-room TV (Toshiba,
      # 1080p@50 over DP-4) stacked above the internal panel. Internal
      # stays primary at the bottom; the TV sits on top for the bike app.
      "zwift" = {
        fingerprint = {
          "DP-4" =
            "00ffffffffffff005262080101010101ff150103805932780a303da1544a9b260f474aadcf0081c08180614c01010101010101010101023a80d072382d40102c458075f23100001e011d80d0721c1620102c258075f23100009e000000fc00544f53484942412d54560a2020000000fd00314c0f510e000a20202020202001a102032871521f2114131216111510050403070206012022230907076c030c001000001ec015151f1f011d00bc52d01e20b828554075f23100001e023a801871382d40582c450075f23100001e011d8018711c1620582c250075f23100009e011d007251d01e206e28550075f23100001e0000000000000000000000000000002c";
          "eDP-1" = internalDisplayG8;
        };
        config = {
          "DP-5".enable = false;
          "DP-4" = {
            enable = true;
            crtc = 1;
            position = "0x0";
            mode = "1920x1080";
            rate = "50.00";
          };
          "eDP-1" = {
            enable = true;
            crtc = 0;
            primary = true;
            position = "0x1080";
            mode = "1920x1200";
            rate = "60.10";
          };
        };
        hooks.postswitch = ''
          ${pkgs.feh}/bin/feh --no-fehbg --bg-fill ${pkgs.markus-wallpapers.orange-cube-internal}
        '';
      };
    };
}
