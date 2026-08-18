# Harness-neutral agent skills (SKILL.md format), packaged as one derivation
# per skill. Consumed by the claude-code (~/.claude/skills) and pi
# (~/.agents/skills) home-manager modules, exposed on the flake as
# `.#agentSkills.<name>` and built wholesale by `checks.agent-skills`.
#
# Validation happens INSIDE each derivation: missing/empty frontmatter, a
# shell script failing `shellcheck --severity=error`, or python that does
# not compile fails the BUILD — `nix flake check` inherits all of it.
#
# `marginalSrc` and `hocketSrc` are the marginal and hocket flake inputs, so
# the marginal-last / hocket-rpc skills and the pkgs.marginal / pkgs.hocket
# binaries they drive are version-locked by flake.lock.
#
# `agentBrowser` is the llm-agents.nix package, not a source tree: upstream
# ships its own SKILL.md inside the binary's $out, so sourcing the skill from
# the package makes text and binary the same derivation — the skew is gone by
# construction and there is nothing left to patch. Passing null omits the
# skill (nuc, which has no chromium and should not pull the closure).
{
  pkgs,
  marginalSrc,
  hocketSrc,
  agentBrowser ? null,
}:

let
  inherit (pkgs) lib;

  mkAgentSkill =
    {
      name,
      src,
      patches ? [ ],
      postPatch ? "",
      harnesses ? [
        "claude"
        "pi"
      ],
    }:
    pkgs.stdenvNoCC.mkDerivation {
      name = "agent-skill-${name}";
      inherit src patches postPatch;

      nativeBuildInputs = [
        pkgs.shellcheck
        pkgs.python3
      ];

      dontConfigure = true;
      dontBuild = true;

      doCheck = true;
      checkPhase = ''
        runHook preCheck

        [ -f SKILL.md ] || { echo "ERROR: ${name}: no SKILL.md"; exit 1; }
        [ "$(head -n1 SKILL.md)" = "---" ] \
          || { echo "ERROR: ${name}: SKILL.md does not start with frontmatter"; exit 1; }
        awk '/^---[[:space:]]*$/{c++; next} c==1' SKILL.md > "$TMPDIR/frontmatter"
        grep -Eq '^name:[[:space:]]*[^[:space:]]' "$TMPDIR/frontmatter" \
          || { echo "ERROR: ${name}: frontmatter lacks a name"; exit 1; }
        grep -Eq '^description:[[:space:]]*[^[:space:]]' "$TMPDIR/frontmatter" \
          || { echo "ERROR: ${name}: frontmatter lacks a description"; exit 1; }

        # Nix-shebang scripts (#!/usr/bin/env nix ... --command bash) are
        # bash but shellcheck cannot infer that from the shebang.
        run_shellcheck() {
          if head -n1 "$1" | grep -q '^#!/usr/bin/env nix'; then
            shellcheck --severity=error --shell=bash "$1"
          else
            shellcheck --severity=error "$1"
          fi
        }
        while IFS= read -r -d "" f; do
          case "$f" in
            *.py) python3 -m py_compile "$f" ;;
            *.sh) run_shellcheck "$f" ;;
            *)
              # extensionless executables (launchers like marginal-last)
              if [ -x "$f" ] && head -n1 "$f" | grep -Eq '^#!.*(bash|/sh|/env nix)'; then
                run_shellcheck "$f"
              fi
              ;;
          esac
        done < <(find . -type f -not -path '*/__pycache__/*' -print0)

        runHook postCheck
      '';

      # py_compile above writes __pycache__; editor litter may ship in src.
      # Neither reaches $out.
      installPhase = ''
        runHook preInstall
        find . -type d -name __pycache__ -prune -exec rm -rf {} +
        find . -type f \( -name '*~' -o -name '#*#' -o -name '.#*' \) -delete
        mkdir -p "$out"
        cp -a ./. "$out/"
        runHook postInstall
      '';

      passthru = { inherit harnesses; };
    };

  # Only directories with a SKILL.md are skills; any support-material
  # sibling is skipped rather than built.
  localSkills =
    lib.mapAttrs
      (
        name: _:
        mkAgentSkill {
          inherit name;
          src = ./. + "/${name}";
        }
      )
      (
        lib.filterAttrs (
          name: type: type == "directory" && builtins.pathExists (./. + "/${name}/SKILL.md")
        ) (builtins.readDir ./.)
      );

  # Skills sourced from other repos via flake inputs, optionally patched.
  # marginal-last is our own upstream: "patching" it means committing there.
  webSkills = {
    marginal-last = mkAgentSkill {
      name = "marginal-last";
      src = marginalSrc + "/launchers/claude-code";
    };

    # Same shape: our own upstream, so the skill text and the `hocket
    # agent` client it calls ride one flake input and cannot drift.
    hocket-rpc = mkAgentSkill {
      name = "hocket-rpc";
      src = hocketSrc + "/skills/hocket-rpc";
    };
  }
  // lib.optionalAttrs (agentBrowser != null) {
    # Taken verbatim from the binary's own $out — no patch. The previous
    # NixOS install note told the agent to `nix run` an unpinned flake,
    # which drifted (ten versions in the store), claimed a bundled Chrome
    # that was really an imperative ~/.agent-browser download, and matched
    # neither pattern in upstream's `allowed-tools`, so every call
    # prompted. Installing the package puts `agent-browser` on PATH, which
    # is what that frontmatter already expects.
    agent-browser = mkAgentSkill {
      name = "agent-browser";
      src = agentBrowser + "/share/agent-browser/skills/agent-browser";
    };
  };
in
localSkills // webSkills
