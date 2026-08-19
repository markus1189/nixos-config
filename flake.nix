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
        in
        {
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

      # `nix develop .#xmonad` / `use flake` in nixos-shared/packages/xmonad;
      # replaces the last channel-style shell.nix.
      devShells.x86_64-linux.xmonad =
        let
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
        in
        pkgs.mkShell {
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
}
