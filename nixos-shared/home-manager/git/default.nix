{
  lib,
  pkgs,
  osConfig,
  ...
}:

let
  inherit (pkgs)
    gitFull
    writeText
    writeScript
    writeShellScript
    runCommandLocal
    stdenv
    gitleaks
    xxd
    ;

  gitPackage = gitFull;

  # The secret half of an OpenPGP key lives on exactly one machine, so there is
  # no single fingerprint that signs everywhere -- pick by host.
  signingKeys = {
    # markus+p1g8, ed25519, generated with the host. Its only uid is
    # markus+p1g8@gmail.com, which is not a deliverable address (gmail
    # sub-addressing would need markus1189+p1g8@), so GitHub cannot verify
    # signatures under it until markus1189@gmail.com is added as a second uid:
    #   gpg --quick-add-uid <fpr> 'Markus Hauck <markus1189@gmail.com>'
    p1g8 = "C60110B291B876AFD152F871A850811C1F25B661";
    # UNVERIFIED: the fingerprint this config carried before it was split per
    # host. p1 was unreachable when that split happened, so nobody has checked
    # that its secret half is actually on that machine -- if commits there start
    # failing with "No secret key", this is why.
    nixos-p1 = "A81E5C8BE9DB291A497B2258B76588292D543934";
  };

  signingKey = signingKeys.${osConfig.networking.hostName} or null;

  # Vendored gitignore.io API output (the URL serves generated, unpinnable
  # content). Refresh manually by re-downloading the API URL if ever needed.
  gitignoreGlobal = writeText "gitignore-global-file" ''
    ${lib.readFile ./gitignore-global}
    # undo-tree removed 2026-08; keep until legacy files are cleaned everywhere
    *~undo-tree~
    .direnv
    .metals
    .aider*
    .claude
    CLAUDE.md
    .worktrees
    .plan
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

  # Secret/marker scanner run from the pre-commit hook. Store paths for the
  # binary and config are baked in below so the hook works regardless of PATH
  # (e.g. when magit invokes the commit).
  #
  # gitleaks' -c overrides a repo's own .gitleaks.toml (config precedence:
  # -c > $GITLEAKS_CONFIG > (target)/.gitleaks.toml). So only pass -c when the
  # repo ships no config of its own; otherwise let gitleaks pick up the
  # repo-local .gitleaks.toml and let that repo fully own its ruleset.
  #
  # The global config lives at the stable, conventional path
  # ~/.config/gitleaks/config.toml (installed via the xdg.configFile entry at
  # the bottom of this module), not a hashed store path, so a repo's
  # .gitleaks.toml can extend rather than replace it:
  #     [extend] path = "/home/<you>/.config/gitleaks/config.toml".
  preCommitHook = writeShellScript "pre-commit" ''
    set -euo pipefail
    root="$(${gitPackage}/bin/git rev-parse --show-toplevel)"
    args=(git --pre-commit --staged --no-banner --redact)
    if [ ! -f "$root/.gitleaks.toml" ]; then
      cfg="$HOME/.config/gitleaks/config.toml"
      if [ ! -f "$cfg" ]; then
        echo "pre-commit: global gitleaks config missing at $cfg (run nh os switch)" >&2
        exit 1
      fi
      args+=(-c "$cfg")
    fi
    exec ${gitleaks}/bin/gitleaks "''${args[@]}"
  '';

  # git's init.templatedir seeds new repos from a directory of hooks. Copy the
  # static hooks verbatim, then drop in the generated gitleaks pre-commit.
  gitTemplateDir = runCommandLocal "git-template-dir" { } ''
    cp -r ${./git_template} $out
    chmod -R u+w $out
    cp ${preCommitHook} $out/hooks/pre-commit
    chmod +x $out/hooks/pre-commit
  '';

  userName = "Markus Hauck";
in
{
  programs.git = {
    enable = true;
    package = gitPackage;
    lfs.enable = true;

    attributes = [
      "*.bin diff=binary"
    ];

    settings = {
      user = {
        name = userName;
        email = "markus1189@gmail.com";
        useConfigOnly = true;
        signingkey = signingKey;
      };

      alias = {
        co = "checkout";
        rh = "reset --hard";
        f = "fetch --all --tags";
        fp = "fetch --all --tags --prune";
        s = "status";
        zip = "archive -o zipped-repository.zip -9";
        lg = "!${gitPrettyLog}";
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
        algorithm = "histogram";
        colormoved = "default";
        colormovedws = "allow-indentation-change";
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
        instructionFormat = "%<(100,trunc)%s | %<(25,trunc)%aN | %<(23,trunc)%ar";
      };

      commit = {
        template = "${./git-commit-template}";
        verbose = true;
        # Signed by default: nuc rebuilds from this repo nightly and activates
        # it as root, so the signature is the only thing distinguishing "Markus
        # pushed" from "someone with a GitHub token pushed".
        gpgsign = signingKey != null;
      };

      init = {
        templatedir = "${gitTemplateDir}";
        defaultBranch = "main";
      };

      transfer.fsckobjects = true;
      fetch.fsckobjects = true;
      receive.fsckobjects = true;

      branch.sort = "-committerdate";
    };
  };

  # Global gitleaks config at the conventional path, referenced by the
  # pre-commit hook above and available for repos to `[extend]`.
  xdg.configFile."gitleaks/config.toml".source = ./gitleaks.toml;
}
