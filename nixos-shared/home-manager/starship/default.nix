{ ... }:
{
  programs.starship = {
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
      # Probes with `sudo -n true` on every prompt; without cached credentials
      # each probe logs "a password is required" to the journal (94 in one day).
      # Fixed upstream in #7530, unreleased as of 1.26.0.
      sudo = {
        disabled = true;
      };
      # Otherwise shows the active gcloud account on every prompt.
      gcloud = {
        detect_env_vars = [ "CLOUDSDK_CORE_PROJECT" ];
      };
      # Only the failure state: the default prints "loaded/allowed" in every .envrc dir.
      direnv = {
        disabled = false;
        format = "([$loaded$allowed]($style) )";
        loaded_msg = "";
        allowed_msg = "";
        unloaded_msg = "direnv not loaded";
        not_allowed_msg = " (not allowed)";
        denied_msg = " (denied)";
      };
      git_metrics = {
        disabled = false;
      };
      battery = {
        display = [
          {
            threshold = 20;
            style = "bold red";
          }
        ];
      };
    };
  };
}
