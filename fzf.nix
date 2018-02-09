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
      interactiveShellInit = ''
        function fzf-history-widget() {
          local selected num
          setopt localoptions noglobsubst noposixbuiltins pipefail 2> /dev/null
          selected=( $(fc -l 1 |
            FZF_DEFAULT_OPTS="--height 40% $FZF_DEFAULT_OPTS --tac -n2..,.. --tiebreak=index --bind=ctrl-r:toggle-sort $FZF_CTRL_R_OPTS --query=''${(q)LBUFFER} +m" fzf) )
          local ret=$?
          if [ -n "$selected" ]; then
            num=$selected[1]
            if [ -n "$num" ]; then
              zle vi-fetch-history -n $num
            fi
          fi
          zle redisplay
          typeset -f zle-line-init >/dev/null && zle zle-line-init
          return $ret
        }
        zle     -N   fzf-history-widget
        bindkey '^R' fzf-history-widget
        bindkey '^r' fzf-history-widget

        ###### CTRL+O: select file via fzf

        __fsel() {
          local cmd="command find -L . -mindepth 1 \\( -path '*/\\.*' -o -fstype 'sysfs' -o -fstype 'devfs' -o -fstype 'devtmpfs' -o -fstype 'proc' \\) -prune \
            -o -type f -print \
            -o -type d -print \
            -o -type l -print 2> /dev/null | cut -b3-"
          setopt localoptions pipefail 2> /dev/null
          eval "$cmd" | FZF_DEFAULT_OPTS="--height 40% --reverse $FZF_DEFAULT_OPTS" fzf -m "$@" | while read item; do
            echo -n "''${(q)item} "
          done
          local ret=$?
          echo
          return $ret
        }

        fzf-file-widget() {
          LBUFFER="''${LBUFFER}$(__fsel)"
          local ret=$?
          zle redisplay
          typeset -f zle-line-init >/dev/null && zle zle-line-init
          return $ret
        }
        zle     -N   fzf-file-widget
        bindkey '^O' fzf-file-widget
      '';
    };
  };
}
