{ config, pkgs, ...}:

{
  environment = {
    systemPackages = with pkgs; [ fzf ];
    interactiveShellInit = ''
      export FZF_DEFAULT_OPTS="--height 30% \
        --reverse \
        --border \
        --color=dark,pointer:232,hl:208,hl+:208,prompt:10,spinner:10,info:10 \
        --bind 'ctrl-y:execute-silent(echo {} | xclip -i -selection clipboard)+abort'"
    '';
  };

  programs = {
    zsh = {
      interactiveShellInit = builtins.readFile "${pkgs.fzf}/share/fzf/key-bindings.zsh";
    };
  };
}
