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
    # nixos-shared/packages/emacs). hurl is the whole CLI repo pulled in
    # for contrib/emacs/hurl-mode.el alone.
    dired-plus = {
      url = "github:emacsmirror/dired-plus";
      flake = false;
    };
    iy-go-to-char = {
      url = "github:doitian/iy-go-to-char";
      flake = false;
    };
    hurl = {
      url = "github:Orange-OpenSource/hurl";
      flake = false;
    };

    # Rust CLI for span-level markdown annotation. Consumed via its own flake
    # output (packages.marginal) but built against our nixpkgs for one toolchain.
    marginal = {
      url = "github:markus1189/marginal";
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
            marginalSrc = inputs.marginal;
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
