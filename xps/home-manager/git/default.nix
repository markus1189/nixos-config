{ fetchurl
, writeText
, gitAndTools
, lib
, writeScript
, stdenv
}:

let
  gitPackage = gitAndTools.gitFull;
  gitignore-io = fetchurl {
    url = "https://www.gitignore.io/api/sbt,git,svn,vim,rust,java,linux,macos,maven,xcode,scala,emacs,gradle,eclipse,windows,haskell,intellij+all,purescript,node,go";
    name = "gitignore-io";
    sha256 = "sha256:05qh93sidr8lwqdd7q600f44yfaxkd4ak739j2crifqg484yqpc3";
  };

  gitignoreGlobal = writeText "gitignore-global-file" ''
    ${lib.readFile gitignore-io}
    *~undo-tree~
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
in
{
  value = {
    enable = true;
    package = gitPackage;
    userEmail = "markus1189@gmail.com";
    userName = "Markus Hauck";

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

    extraConfig = {
      user = {
        useConfigOnly = true;
        signingkey = "A7767559";
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

      difftool = {
        prompt = false;
      };

      hooks = {
        allowunannotated = false;
      };

      help = {
        autocorrect = 1;
      };

      push = {
        default = "tracking";
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
      };
    };
  };
}
