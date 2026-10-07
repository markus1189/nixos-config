{
  config,
  lib,
  pkgs,
  ...
}:

{
  # Fires on the first rebuild after nixpkgs ships tmux 3.8; delete it once acted on.
  warnings = lib.optional (lib.versionAtLeast pkgs.tmux.version "3.8") ''
    tmux ${pkgs.tmux.version} records OSC 133 D exit status: try #{pane_command_status},
    #{pane_command_duration} and the pane-command-finished hook (per-pane failure
    display, agent-pane sparkline in status-right), and retire the shell half of
    nixos-shared/packages/tmux/semantic-prompt.nix in favour of Ghostty's integration.
  '';

  services = {
    xserver = {
      displayManager = {
        sessionCommands = ''
          ${pkgs.tmux}/bin/tmux new-session -d -s immortals || true &
          ${pkgs.tmux}/bin/tmux new-session -d -s default || true &
          ${pkgs.tmux}/bin/tmux new-session -d -s im || true &
        '';
      };
    };
  };

  environment = {
    systemPackages = with pkgs; [ tmux ];
  };

  programs = {
    tmux = {
      enable = true;
      baseIndex = 1;
      clock24 = true;
      keyMode = "vi";

      extraConfig =
        with pkgs;
        let
          popupScratch = pkgs.writeShellScript "popup-scratch" (pkgs.lib.readFile ./popup-scratch.sh);
          nextBell = pkgs.writeShellScript "tmux-next-bell" (pkgs.lib.readFile ./next-bell.sh);
          silenceNotify = pkgs.writeShellScript "tmux-silence-notify" (
            builtins.replaceStrings [ "@dunstify@" ] [ "${pkgs.dunst}/bin/dunstify" ] (
              pkgs.lib.readFile ./silence-notify.sh
            )
          );
        in
        ''
          ${builtins.replaceStrings
            [ "@popup-scratch@" "@next-bell@" "@silence-notify@" ]
            [ "${popupScratch}" "${nextBell}" "${silenceNotify}" ]
            (builtins.readFile ./tmux.conf)
          }
          run-shell ${tmuxPlugins.yank}/share/tmux-plugins/yank/yank.tmux

          set -g @extrakto_copy_key "enter"
          set -g @extrakto_insert_key "tab"
          set -g @extrakto_key "e"
          set -g @extrakto_grab_area "recent"
          set -g @extrakto_filter_order "path url line word quote s-quote all"
          run-shell ${tmuxPlugins.extrakto}/share/tmux-plugins/extrakto/extrakto.tmux

          run-shell ${tmuxPlugins.fingers}/share/tmux-plugins/tmux-fingers/tmux-fingers.tmux
        '';
    };
  };
}
