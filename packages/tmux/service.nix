{ config, pkgs, ...}:

{
  services = {
    xserver = {
      displayManager = {
        sessionCommands = ''
          tmux new-session -d -s immortals || true &
          tmux new-session -d -s default || true &
          tmux new-session -d -s im || true &
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
      extraTmuxConf = pkgs.lib.readFile ./tmux.conf;
    };
  };
}
