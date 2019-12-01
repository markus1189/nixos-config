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
      extraTmuxConf = ''
        ${pkgs.lib.readFile ./tmux.conf}
        run-shell ${pkgs.tmuxPlugins.yank}/share/tmux-plugins/yank/yank.tmux
        run-shell ${pkgs.tmuxPlugins.copycat}/share/tmux-plugins/copycat/copycat.tmux
      '';
    };
  };
}
