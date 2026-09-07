{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

{
  # Installs nix-index and comma wrapped around the prebuilt database from
  # flake.lock, wires the command-not-found handler into bash/zsh/fish, and
  # defaults programs.command-not-found.enable to false.
  imports = [ inputs.nix-index-database.nixosModules.nix-index ];

  programs = {
    bcc.enable = true; # shellsnoop, opensnoop, exitsnoop etc

    nix-index-database.comma.enable = true;

    # nh reimplements nixos-rebuild in Rust: a nix-output-monitor build tree, a
    # closure diff, then a confirmation prompt before activation -- the
    # `nixos-rebuild build` / `nix store diff-closures` / `switch` sequence that
    # docs/derivation-diffing.md spells out by hand, as one command.
    #
    # The diff comes from dix linked as a *library crate* (crates/nh-diff), not
    # from the `dix` binary, so the standalone CLI in common-packages.nix is a
    # separate tool for ad-hoc `dix <genA> <genB>` -- not a dependency of this.
    nh = {
      enable = true;
      # Default target for a bare `nh os switch`, so it resolves from any
      # directory. The host attr still comes from the hostname unless -H says
      # otherwise -- p1's nixos-p1 alias in flake.nix keeps working. mkDefault
      # because nuc has no local checkout worth building and overrides it.
      flake = lib.mkDefault "/home/${config.my.userName}/repos/nixos-config";

      # Off everywhere: on the laptops nix.gc.automatic is false and collection
      # stays a manual decision, on nuc it is already automatic at 03:15. Run it
      # by hand when wanted:
      #   nh clean all --keep-since 5d --keep 3
      # It is gcroot- and direnv-aware, which plain nix-collect-garbage is not.
      clean.enable = false;
    };

    firejail.enable = true;

    less = {
      envVariables = {
        LESS = "-RXi";
      };
    };

    bash = {
      completion.enable = true;
      enableLsColors = true;

    };

    java = {
      enable = true;
      package = pkgs.temurin-bin;
    };

    gnupg = {
      agent = {
        enable = true;
      };
    };

    npm = {
      npmrc = ''
        ignore-scripts=true
      '';
    };
  };
}
