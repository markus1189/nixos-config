{
  description = "NixOS configurations (p1, p1g8, nuc)";

  inputs = {
    # Tracks the nixos-unstable branch; the exact rev lives in flake.lock.
    # Bump with `nix flake update nixpkgs`.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    # Bleeding-edge packages (claude-code, nix-direnv); bump with
    # `nix flake update nixpkgs-master`.
    nixpkgs-master.url = "github:NixOS/nixpkgs/master";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
      inputs.darwin.follows = "";
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Weekly-rebuilt nix-index database as a store path, so `,` and the
    # command-not-found handler answer from flake.lock instead of from a
    # hand-run `nix-index` in each user's ~/.cache.
    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      # The overlay only uses nixpkgs-stable for its own CI; without the
      # follows it locks a third full nixpkgs tree that is never evaluated.
      inputs.nixpkgs-stable.follows = "nixpkgs";
    };

    # Non-flake source trees.
    # darktable needs its submodules (rawspeed etc.) — a github: tarball
    # input would silently drop them.
    darktable = {
      url = "git+https://github.com/darktable-org/darktable?submodules=1";
      flake = false;
    };
    # Maintained fork of clvv/fasd; nixpkgs removed the package after the
    # original repo was archived (built in nixos-shared/packages/fasd).
    fasd = {
      url = "github:whjvenyl/fasd";
      flake = false;
    };
    visidata = {
      url = "github:saulpw/visidata/develop";
      flake = false;
    };
    xclip = {
      url = "github:astrand/xclip";
      flake = false;
    };
    gptel = {
      url = "github:karthink/gptel";
      flake = false;
    };
    stevenblack-hosts = {
      url = "github:StevenBlack/hosts";
      flake = false;
    };
    zsh-histdb = {
      url = "github:larkery/zsh-histdb";
      flake = false;
    };
    # Single-file elisp packages without a MELPA recipe (consumed in
    # nixos-shared/packages/emacs).
    iy-go-to-char = {
      url = "github:doitian/iy-go-to-char";
      flake = false;
    };

    # Rust CLI for span-level markdown annotation. Consumed via its own flake
    # output (packages.marginal) but built against our nixpkgs for one toolchain.
    marginal = {
      url = "github:markus1189/marginal";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Haskell TUI for Raindrop bookmarks (own flake, packages.hocket). One
    # input serves both the binary and the hocket-rpc agent skill, so the
    # control-socket protocol the skill documents and the `hocket agent`
    # client that speaks it are the same rev by construction.
    hocket = {
      url = "github:markus1189/hocket";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # agent-browser as a runnable binary, whose own $out/share/agent-browser
    # also carries the SKILL.md that nixos-shared/agent-skills ships — so the
    # skill text and the binary it describes are one derivation, not two
    # inputs that drift. Overrides pkgs.agent-browser (nixpkgs is stuck on
    # 0.27.0 from 2026-05; upstream ships every few days and this flake is
    # bumped daily).
    # The `follows` is deliberate: it makes our nixpkgs config apply and keeps
    # one chromium instead of two. It currently costs no cache hits either —
    # with it, the derivation still resolves to the same store path numtide's
    # own cache serves, so no numtide substituter/key is needed here.
    llm-agents = {
      url = "github:numtide/llm-agents.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{ self, nixpkgs, ... }:
    let
      mkHost =
        modules:
        nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = { inherit inputs; };
          modules = [
            inputs.home-manager.nixosModules.home-manager
            inputs.agenix.nixosModules.default
            ./nixos-shared/flake-base.nix
          ]
          ++ modules;
        };
    in
    {
      nixosConfigurations = {
        p1 = mkHost [ ./p1/configuration.nix ];
        p1g8 = mkHost [
          ./p1g8/configuration.nix
          inputs.disko.nixosModules.disko
        ];
        nuc = mkHost [ ./nuc/configuration.nix ];
        # p1's hostname is nixos-p1; alias so `nixos-rebuild --flake .`
        # resolves without an explicit attr name.
        nixos-p1 = self.nixosConfigurations.p1;
      };

      # `nix fmt` formats the whole tree with RFC-style nixfmt via treefmt
      # (bare nixfmt reads stdin / chokes on the ./result symlink).
      formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixfmt-tree;

      # Scripts as `nix run .#myScripts.<name>` / `nix build .#myScripts.<name>`
      # without a host eval (legacyPackages: the flat `packages` schema forbids
      # nesting). Same overlays as the hosts apply — including the custom
      # packages overlay itself — so all 55 attrs are drv-identical to the
      # host-installed scripts (verified by diff after #24).
      # filterAttrs drops the 11 function attrs (writeShellScript & co).
      legacyPackages.x86_64-linux =
        let
          pkgs = import nixpkgs {
            system = "x86_64-linux";
            config.allowUnfree = true;
            overlays = [
              inputs.emacs-overlay.overlays.default
            ]
            ++ import ./nixos-shared/shared-overlays.nix inputs
            ++ [ (import ./nixos-shared/packages/overlay.nix inputs) ];
          };
        in
        {
          myScripts = nixpkgs.lib.filterAttrs (_: nixpkgs.lib.isDerivation) pkgs.myScripts;

          # Agent skills as `nix build .#agentSkills.<name>` — the fast
          # no-sudo iteration check; validation runs inside each build.
          agentSkills = import ./nixos-shared/agent-skills {
            inherit pkgs;
            # This pkgs carries the scripts overlay, not flake-base's, so
            # there is no pkgs.marginal here — same reason agent-browser is
            # reached through its input below. `inputs.nixpkgs.follows` makes
            # it the derivation the hosts install regardless.
            marginal = inputs.marginal.packages.x86_64-linux.marginal;
            hocketSrc = inputs.hocket;
            agentBrowser = inputs.llm-agents.packages.x86_64-linux.agent-browser;
          };
        };

      # The two bats suites, gated by `nix flake check` instead of human whim.
      # They source their script-under-test via $BATS_TEST_DIRNAME, so the
      # whole claude/ tree is the test fixture.
      checks.x86_64-linux =
        let
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          batsWith = pkgs.bats.withLibraries (p: [
            p.bats-assert
            p.bats-support
          ]);

          # Sources for both linters, narrowed to *.nix + statix.toml with
          # lib.fileset, so editing emacs-config.el does not re-run either.
          lintTree = nixpkgs.lib.fileset.toSource {
            root = ./.;
            fileset = nixpkgs.lib.fileset.unions [
              (nixpkgs.lib.fileset.fileFilter (f: f.hasExt "nix") ./.)
              ./statix.toml
            ];
          };
        in
        {
          # statix lints every .nix file in the tree; which lints and which
          # files are exempt lives in statix.toml at the repo root (shared
          # with the editor's flymake backend, so both judge the same way).
          statix = pkgs.runCommand "statix-check" { nativeBuildInputs = [ pkgs.statix ]; } ''
            statix check --config ${lintTree} --unrestricted ${lintTree}
            touch $out
          '';

          # deadnix is the complement to statix: statix finds antipatterns,
          # deadnix finds bindings nothing references.
          #
          # --no-lambda-pattern-names is mandatory here, not a preference.
          # Every NixOS and home-manager module declares `{ config, pkgs, ... }`
          # whether or not it uses each arg; without the flag this tree reports
          # 100 findings, 87 of them that convention. Upstream's own wording for
          # the flag is "don't break nixpkgs `callPackage`". With it: 13, all
          # real, all now fixed.
          #
          # Hidden directories are skipped by default (deadnix needs --hidden to
          # descend), so .direnv/flake-inputs -> nixpkgs is not walked. That is
          # the same trap statix.toml's `ignore` exists to close.
          deadnix = pkgs.runCommand "deadnix-check" { nativeBuildInputs = [ pkgs.deadnix ]; } ''
            deadnix --no-lambda-pattern-names --fail ${lintTree}
            touch $out
          '';

          # Builds (= validates: frontmatter, shellcheck, py_compile) every
          # agent skill; the farm shape doubles as the future whole-dir target.
          agent-skills = pkgs.linkFarm "agent-skills" (
            nixpkgs.lib.mapAttrsToList (name: drv: {
              inherit name;
              path = drv;
            }) self.legacyPackages.x86_64-linux.agentSkills
          );

          claude-statusline-bats =
            pkgs.runCommand "claude-statusline-bats"
              {
                nativeBuildInputs = [
                  batsWith
                  pkgs.jq
                  pkgs.bc
                  pkgs.git
                ];
              }
              ''
                cd ${./nixos-shared/claude}
                HOME=$TMPDIR bats claude-code-statusline.bats
                touch $out
              '';

          claude-hooks-bats =
            pkgs.runCommand "claude-hooks-bats"
              {
                nativeBuildInputs = [
                  batsWith
                  pkgs.jq
                  pkgs.ast-grep
                ];
              }
              ''
                cd ${./nixos-shared/claude}
                HOME=$TMPDIR bats hooks/check-dangerous-commands.bats
                touch $out
              '';
        };

      devShells.x86_64-linux =
        let
          pkgs = nixpkgs.legacyPackages.x86_64-linux;

          # The repo's pre-commit hook, generated rather than checked in: the
          # linters that gate a commit are then the ones from this flake's
          # nixpkgs -- the same derivations `nix flake check` runs -- instead of
          # whatever the ambient PATH happens to offer.
          #
          # `.git/hooks` is not tracked and cannot be, so the install has to
          # happen somewhere. The devShell's shellHook is that somewhere: enter
          # the directory with direnv (.envrc: use_flake) and the symlink is
          # made, re-made after a GC, and corrected if it points at an older
          # revision of this hook.
          preCommitHook = pkgs.writeShellScript "nixos-config-pre-commit" ''
            set -euo pipefail
            root="$(git rev-parse --show-toplevel)"

            # statix and deadnix, only when the commit touches .nix files. Both
            # parse rather than evaluate, so linting the whole tree costs ~10ms;
            # there is no point being clever about which files to hand them.
            #
            # The whole-repo walk is deliberate: `ignore` globs in statix.toml
            # are honoured for a directory walk but NOT for an explicit file
            # target (statix 0.5.8), so a per-file loop would lint the generated
            # hardware-configuration.nix that statix.toml exempts. The price is
            # that a lint in an unstaged file blocks the commit too.
            if [ -n "$(git diff --cached --name-only --diff-filter=ACM -- '*.nix')" ]; then
              if ! ${pkgs.statix}/bin/statix check --config "$root" "$root"; then
                echo "pre-commit: statix found lints -- fix with 'statix fix', or commit with --no-verify" >&2
                exit 1
              fi

              # --no-lambda-pattern-names: see checks.deadnix for why this is
              # mandatory rather than taste. Hidden dirs are skipped by default,
              # so .direnv needs no exclusion here the way it does for statix.
              if ! ${pkgs.deadnix}/bin/deadnix --no-lambda-pattern-names --fail "$root"; then
                echo "pre-commit: deadnix found dead code -- fix with 'deadnix --no-lambda-pattern-names --edit', or commit with --no-verify" >&2
                exit 1
              fi
            fi

            # Hand over to the machine-wide hook (gitleaks), which this one
            # displaces. Resolved at runtime so it always chains to the current
            # one. Fail closed: a missing secret scanner on a public repo is a
            # reason not to commit, not a reason to shrug.
            template="$(git config --get init.templatedir || true)"
            if [ -z "$template" ] || [ ! -x "$template/hooks/pre-commit" ]; then
              echo "pre-commit: no template pre-commit hook via init.templatedir -- refusing to commit without the gitleaks scan (run ./activate.sh)" >&2
              exit 1
            fi
            exec "$template/hooks/pre-commit"
          '';
        in
        {
          # `nix develop` / direnv in the repo root. Carries the linter and
          # installs the pre-commit hook that runs it.
          default = pkgs.mkShell {
            packages = [
              pkgs.statix
              pkgs.deadnix
            ];

            # git resolves hooks under $GIT_COMMON_DIR, so --git-path gets this
            # right inside a linked worktree too, where .git is a file.
            shellHook = ''
              hooksDir="$(git rev-parse --git-path hooks 2>/dev/null || true)"
              if [ -d "$hooksDir" ]; then
                ln -sfn ${preCommitHook} "$hooksDir/pre-commit"
              fi
            '';
          };

          # `nix develop .#xmonad` / `use flake` in nixos-shared/packages/xmonad;
          # replaces the last channel-style shell.nix.
          xmonad = pkgs.mkShell {
            packages = [
              (pkgs.haskellPackages.ghcWithHoogle (
                ps: with ps; [
                  xmonad
                  xmonad-contrib
                  haskell-language-server
                ]
              ))
            ];
          };
        };
    };
}
