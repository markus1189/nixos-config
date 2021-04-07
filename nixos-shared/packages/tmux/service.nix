{ config, pkgs, ...}:

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

  environment = {
    systemPackages = with pkgs; [ tmux ];
  };

  programs = {
    tmux = {
      enable = true;
      baseIndex = 1;
      clock24 = true;
      keyMode = "vi";

      extraConfig = with pkgs; ''
        ${pkgs.lib.readFile ./tmux.conf}
        run-shell ${tmuxPlugins.yank}/share/tmux-plugins/yank/yank.tmux
        run-shell ${tmuxPlugins.copycat}/share/tmux-plugins/copycat/copycat.tmux
        run-shell ${tmuxPlugins.fpp}/share/tmux-plugins/fpp/fpp.tmux
        run-shell ${tmuxPlugins.fingers}/share/tmux-plugins/fingers/tmux-fingers.tmux
        run-shell ${tmuxPlugins.extrakto}/share/tmux-plugins/extrakto/extrakto.tmux
      '';
    };
  };
}
