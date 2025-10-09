{ }:
{
  value = {
    enable = true;
    settings = {
      time = {
        disabled = false;
        format = "[$time]($style) ";
      };
      cmd_duration = {
        show_notifications = false; # Custom zsh functionality shows exit code already
      };
      status = {
        disabled = false;
      };
      shlvl = {
        disabled = false;
        threshold = 3;
      };
      sudo = {
        disabled = false;
      };
    };
  };
}
