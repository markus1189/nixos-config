{ config, pkgs, ... }:

{
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

  environment = { systemPackages = with pkgs; [ tmux ]; };

  programs = {
    tmux = {
      enable = true;
      baseIndex = 1;
      clock24 = true;
      keyMode = "vi";

      extraConfig = with pkgs; let
        popupScratch = pkgs.writeShellScript "popup-scratch" (pkgs.lib.readFile ./popup-scratch.sh);
        nextBell = pkgs.writeShellScript "tmux-next-bell" (pkgs.lib.readFile ./next-bell.sh);
      in ''
        ${builtins.replaceStrings
            ["@popup-scratch@" "@next-bell@"]
            ["${popupScratch}" "${nextBell}"]
            (builtins.readFile ./tmux.conf)}
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
