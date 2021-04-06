{ fetchurl
, writeText
, gitAndTools
, lib
, writeScript
, stdenv
, ndtSources
, xxd
}:

let
  gitPackage = gitAndTools.gitFull;

  gitignoreGlobal = writeText "gitignore-global-file" ''
    ${lib.readFile ndtSources.gitignore-io}
    *~undo-tree~
    .direnv
  '';

  gitPrettyLog = writeScript "git-pretty-log.sh" ''
    #!${stdenv.shell}

    HASH="%C(yellow)%h%Creset"
    RELATIVE_TIME="%Cgreen(%ar)%Creset"
    AUTHOR="%C(bold blue)<%aN>%Creset"
    REFS="%C(red)%d%Creset"
    SUBJECT="%s"
    GPG="%C(yellow)%G?%Creset"

    FORMAT="''${HASH} ''${RELATIVE_TIME} ''${AUTHOR} ''${REFS} ''${SUBJECT}"

    pretty_git_log() {
      ${gitPackage}/bin/git log --graph --color --pretty="tformat:''${FORMAT}" $* |
        sed -Ee 's/(^[^<]*) ago\)/\1)/' |
        sed -Ee 's/(^[^<]*), [[:digit:]]+ .*months?\)/\1)/' |
        less -FXRS
    }

    pretty_git_log $*
  '';

  userName = "Markus Hauck";
in
{
  value = {
    inherit userName;
    userEmail = "markus1189@gmail.com";

    enable = true;
    package = gitPackage;

    delta.enable = true;

    aliases = {
      co = "checkout";
      rh = "reset --hard";
      f = "fetch --all --tags";
      fp = "fetch --all --tags --prune";
      s = "status";
      zip = "archive -o zipped-repository.zip -9";
      lg = "!${gitPrettyLog}";
    };

    attributes = [
      "*.bin diff=binary"
    ];

    includes = [
      {
        condition = "gitdir:~/repos/otto/";
        contents = {
          user = {
            name = userName;
            email = "markus.hauck@otto.de";
            signingkey = "6A0A92D088F61D5115036375F51BC654371DD87B";
          };

          commit = {
            gpgsign = true;
          };
        };
      }
    ];

    extraConfig = {
      user = {
        useConfigOnly = true;
        signingkey = "A81E5C8BE9DB291A497B2258B76588292D543934";
      };

      color = {
        ui = "auto";
        branch = "auto";
        diff = "auto";
        interactive = "auto";
        status = "auto";
      };

      "color \"branch\"" = {
        current = "yellow";
        local = "green";
        remote = "red";
      };

      "color \"status\"" = {
        added = "green";
        changed = "red";
        untracked = "yellow";
      };

      core = {
        excludesfile = "${gitignoreGlobal}";
        whitespace = "trailing-space,space-before-tab";
      };

      credential = {
        helper = "cache --timeout=3600";
      };

      diff = {
        mnemonicprefix = true;
      };

      "diff \"binary\"" = {
        textconv = "${xxd}/bin/xxd";
      };

      difftool = {
        prompt = false;
      };

      hooks = {
        allowunannotated = false;
      };

      help = {
        autocorrect = 1;
      };

      pull = {
        rebase = true;
        ff = "only";
      };

      push = {
        default = "current";
      };

      rerere = {
        enabled = true;
      };

      apply = {
        whitespace = "fix";
      };

      rebase = {
        stat = true;
        autosquash = true;
      };

      commit = {
        template = "${./git-commit-template}";
      };

      init = {
        templatedir = "${./git_template}";
        defaultBranch = "main";
      };
    };
  };
}
