{ config, pkgs, ... }:

let
  modifiedZbell = pkgs.writeText "modified-zbell.sh" ''
#!/usr/bin/env zsh
[[ -o interactive ]] || return

# get $EPOCHSECONDS. builtins are faster than date(1)
zmodload zsh/datetime || return

autoload -Uz add-zsh-hook || return

(( ''${+zbell_duration} )) || zbell_duration=15

(( ''${+zbell_ignore} )) || zbell_ignore=($EDITOR $PAGER)

zbell_timestamp=$EPOCHSECONDS

# right before we begin to execute something, store the time it started at
zbell_begin() {
  zbell_timestamp=$EPOCHSECONDS
  zbell_lastcmd=$1
}

# when it finishes, if it's been running longer than $zbell_duration,
# and we dont have an ignored command in the line, then print a bell.
zbell_end() {
        LAST_EC=$?
  ran_long=$(( $EPOCHSECONDS - $zbell_timestamp >= $zbell_duration ))

  has_ignored_cmd=0
  for cmd in ''${(s:;:)zbell_lastcmd//|/;}; do
    words=(''${(z)cmd})
    util=''${words[1]}
    if (( ''${zbell_ignore[(i)$util]} <= ''${#zbell_ignore} )); then
      has_ignored_cmd=1
      break
    fi
  done

  if (( ! $has_ignored_cmd )) && (( ran_long )); then
                if [[ "$LAST_EC" == 0 ]]; then
                  notify-send "Command finished [$LAST_EC]" "$zbell_lastcmd"
                else
                  notify-send -u critical "Command failed [$LAST_EC]" "$zbell_lastcmd"
                fi
    print -n "\a"
  fi
}

add-zsh-hook preexec zbell_begin
add-zsh-hook precmd zbell_end
'';
  customEraseWord = pkgs.writeText "custom-erase-word.zsh" ''
    my-backward-delete-word() {
      local WORDCHARS=''${WORDCHARS/\//}
      local WORDCHARS=''${WORDCHARS/[.]/}
      zle backward-delete-word
    }
    zle -N my-backward-delete-word
    bindkey '^W' my-backward-delete-word
  '';
in
{
  programs = {
    zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;
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
        if [ "$IN_NIX_SHELL" = 1 ] ; then
          RPROMPT+="%{$fg_bold[red]%}NIX %{$reset_color%}"
        fi
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

        source ${modifiedZbell}
        source ${customEraseWord}
      '';
    };
  };
}
