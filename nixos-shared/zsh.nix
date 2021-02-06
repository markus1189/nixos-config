{ config, pkgs, ... }:

let
  customEraseWord = pkgs.writeText "custom-erase-word.zsh" ''
    # TODO: unfortunately makes M-W etc not work for autosuggestions
    my-backward-delete-word() {
      local WORDCHARS=''${WORDCHARS/\//}
      local WORDCHARS=''${WORDCHARS/[.]/}
      local WORDCHARS=''${WORDCHARS/[-]/}
      local WORDCHARS=''${WORDCHARS/[_]/}
      zle backward-delete-word
    }
    zle -N my-backward-delete-word
    bindkey '^W' my-backward-delete-word

    my-backward-word() {
      local WORDCHARS=''${WORDCHARS/\//}
      local WORDCHARS=''${WORDCHARS/[.]/}
      zle backward-word
    }
    zle -N my-backward-word
    bindkey '^[b' my-backward-word

    my-delete-word() {
      local WORDCHARS=''${WORDCHARS/\//}
      local WORDCHARS=''${WORDCHARS/[.]/}
      zle delete-word
    }
    zle -N my-delete-word
    bindkey 'M-D' my-delete-word

    my-forward-word() {
      local WORDCHARS=''${WORDCHARS/\//}
      local WORDCHARS=''${WORDCHARS/[.]/}
      zle forward-word
    }
    zle -N my-forward-word
    bindkey '^[f' my-forward-word
  '';
in
{
  programs = {
    zsh = {
      enable = true;
      enableCompletion = true;
      autosuggestions.enable = true;
      promptInit = ''
        autoload -U promptinit && promptinit

        PROMPT=""
        PROMPT="%{$fg_bold[blue]%}%2c > %{$reset_color%}"

        RPROMPT=""
        RPROMPT+="%{$fg_bold[blue]%}%n@%m%{$reset_color%} "
        RPROMPT+="%{$fg_bold[yellow]%}%j%{$reset_color%} "
        RPROMPT+="%{$fg_bold[yellow]%}$SHLVL:$(ps --no-headers -o comm $PPID)%{$reset_color%} "
        RPROMPT+="%{$fg_bold[green]%}%h%{$reset_color%} "
        RPROMPT+="%{$fg_bold[magenta]%}%(?..[%?] )%{$reset_color%}"
        RPROMPT+="%{$fg_bold[yellow]%}%D{%H:%M:%S}%{$reset_color%}"
      '';

      syntaxHighlighting = {
        enable = true;
        highlighters = [ "main" "brackets" ];
      };

      interactiveShellInit = ''
        autoload -U colors && colors
        autoload -z edit-command-line

        zstyle ':completion:*' completer _expand _complete _ignored _approximate

        autoload -Uz compinit
        compinit

        HISTFILE=~/.histfile
        HISTSIZE=999999999999
        SAVEHIST=999999999999

        setopt appendhistory
        bindkey -e

        zle -N edit-command-line
        bindkey "^X^E" edit-command-line

        copy-current-command() {
          zle kill-buffer
          print -rn -- $CUTBUFFER | ${pkgs.xclip}/bin/xclip -i -selection clipboard
        }

        zle -N copy-current-command
        bindkey '^X^A' copy-current-command

        source ${modifiedZbell}
        source ${customEraseWord}
      '';
    };
  };
}
