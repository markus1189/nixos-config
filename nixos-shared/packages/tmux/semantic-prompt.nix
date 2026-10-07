{ ... }:
{
  # tmux's previous-prompt/next-prompt find prompts and output only via OSC 133
  # marks; starship 1.26 emits none, and Ghostty's own integration doesn't load
  # inside tmux.
  programs.zsh.interactiveShellInit = ''
    autoload -Uz add-zsh-hook
    _osc133_prompt() { print -n '\e]133;A\e\\' }
    _osc133_output() { print -n '\e]133;C\e\\' }
    add-zsh-hook precmd _osc133_prompt
    add-zsh-hook preexec _osc133_output
  '';
  programs.tmux.extraConfig = ''
    bind O copy-mode \; send -X previous-prompt -o
  '';
}
