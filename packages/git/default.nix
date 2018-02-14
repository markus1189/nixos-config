{mutate, fetchurl, stdenv, git, writeScript, lib, writeText}:

let
  gitignore-io = fetchurl {
    url = "https://www.gitignore.io/api/sbt,git,svn,vim,rust,java,linux,macos,maven,xcode,scala,emacs,gradle,eclipse,windows,haskell,intellij,purescript";
    name = "gitignore-io";
    sha256 = "1w5xwgxpvv4y9p6jj7zda72vfljwm9f8r0g56jwskpx5yaw9vrm7";
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
