# Harness-neutral agent skills (SKILL.md format), packaged as one derivation
# per skill. Consumed by the claude-code (~/.claude/skills) and pi
# (~/.agents/skills) home-manager modules, exposed on the flake as
# `.#agentSkills.<name>` and built wholesale by `checks.agent-skills`.
#
# Validation happens INSIDE each derivation: missing/empty frontmatter, a
# shell script failing `shellcheck --severity=error`, or python that does
# not compile fails the BUILD — `nix flake check` inherits all of it.
#
# `marginalSrc` is the marginal flake input, so the marginal-last skill and
# the pkgs.marginal binary it drives are version-locked by flake.lock.
# Passing null (nix-on-droid, whose flake has no marginal input) simply
# omits the skill.
{
  pkgs,
  marginalSrc ? null,
  agentBrowserSrc ? null,
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

  # Only directories with a SKILL.md are skills; siblings like patches/
  # are support material, not test subjects.
  localSkills = lib.mapAttrs (
    name: _:
    mkAgentSkill {
      inherit name;
      src = ./. + "/${name}";
    }
  ) (
    lib.filterAttrs (
      name: type: type == "directory" && builtins.pathExists (./. + "/${name}/SKILL.md")
    ) (builtins.readDir ./.)
  );

  # Skills sourced from other repos via flake inputs, optionally patched.
  # marginal-last is our own upstream: "patching" it means committing there.
  webSkills =
    lib.optionalAttrs (marginalSrc != null) {
      marginal-last = mkAgentSkill {
        name = "marginal-last";
        src = marginalSrc + "/launchers/claude-code";
      };
    }
    // lib.optionalAttrs (agentBrowserSrc != null) {
      # Foreign upstream: local deltas live as a reviewable patch instead
      # of invisible edits to a vendored copy. If an update breaks the
      # patch, the build fails and the reconciliation is explicit.
      agent-browser = mkAgentSkill {
        name = "agent-browser";
        src = agentBrowserSrc + "/skills/agent-browser";
        patches = [ ./patches/agent-browser-nixos-install.patch ];
      };
    };
in
localSkills // webSkills
