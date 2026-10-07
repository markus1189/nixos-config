{ ... }:
{
  # tmux's previous-prompt/next-prompt find prompts and output only via OSC 133
  # marks; starship 1.26 emits none, and Ghostty's own integration doesn't load
  # inside tmux.
  programs.zsh.interactiveShellInit = ''
    autoload -Uz add-zsh-hook
    # The prompt mark must be part of PS1: zle redraws the prompt line, and
    # tmux drops a mark printed from precmd. Prepended lazily because
    # starship sets PS1 after /etc/zshrc.
    _osc133_prompt() { [[ $PS1 == *']133;A'* ]] || PS1=$'%{\e]133;A\e\\%}'$PS1 }
    _osc133_output() { print -n '\e]133;C\e\\' }
    add-zsh-hook precmd _osc133_prompt
    add-zsh-hook preexec _osc133_output
  '';
  programs.tmux.extraConfig = ''
    bind O copy-mode \; send -X previous-prompt -o
    # Claude Code (2.1.289) emits no OSC 133 outside screen-reader mode, and
    # tmux loses the marks it emits there; its user prompts start with "❯ ".
    bind -T copy-mode-vi [ if -F '#{==:#{pane_current_command},claude}' { send -X search-backward '^❯ ' } { send -X previous-prompt -o }
    bind -T copy-mode-vi ] if -F '#{==:#{pane_current_command},claude}' { send -X search-forward '^❯ ' } { send -X next-prompt -o }
    bind -T copy-mode-vi ( send -X previous-prompt
    bind -T copy-mode-vi ) send -X next-prompt
  '';
}
