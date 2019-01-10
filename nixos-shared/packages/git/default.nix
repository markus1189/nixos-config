{mutate, fetchurl, stdenv, git, writeScript, lib, writeText}:

let
  gitignore-io = fetchurl {
    url = "https://www.gitignore.io/api/sbt,git,svn,vim,rust,java,linux,macos,maven,xcode,scala,emacs,gradle,eclipse,windows,haskell,intellij,purescript,node";
    name = "gitignore-io";
    sha256 = "1hs6dq1d46p8lmx09yhv5bb1z9bc7kwy3jgh7m55cjr6px34k83r";
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
      ${git}/bin/git log --graph --color --pretty="tformat:''${FORMAT}" $* |
        sed -Ee 's/(^[^<]*) ago\)/\1)/' |
        sed -Ee 's/(^[^<]*), [[:digit:]]+ .*months?\)/\1)/' |
        less -FXRS
    }

    pretty_git_log $*
  '';
  in mutate ./gitconfig {
  inherit gitignoreGlobal gitPrettyLog;
  gitTemplate = ./git_template;
  gitCommitTemplate = ./git-commit-template;
}
