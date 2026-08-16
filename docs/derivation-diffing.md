# Derivation and closure diffing

`nix eval` proves a change builds; diffing proves *what* it built. Works on any
two derivations or store paths — a package, a script, a host toplevel. Whole
systems are the expensive special case, not the default.

## Two granularities

| | Command | Build needed? | Sees |
|---|---|---|---|
| **Runtime closure** | `nix store diff-closures BEFORE AFTER` | yes, both sides | package name + version deltas, closure size |
| **Derivation closure** | `nix-store -qR DRV` | no, eval only | everything: build-only inputs, generated config text, patches |

`diff-closures` is the readable summary but blind to a regenerated config file
whose dependencies didn't move. The drv closure is ground truth and costs an
eval — reach for it first, build only for the human-readable version delta.

```bash
# No build at all: running system vs working tree
old=$(nix-store --query --deriver /run/current-system)   # still on disk for the current generation
new=$(nix eval --raw .#nixosConfigurations.p1g8.config.system.build.toplevel.drvPath)
diff <(nix-store -qR "$old" | sort) <(nix-store -qR "$new" | sort)

# Built delta
nixos-rebuild build --flake .#p1g8 && nix store diff-closures /run/current-system ./result
nix build .#nixosConfigurations.p1g8.config.system.build.toplevel -o result-after  # second tree, keeps ./result
```

Other baselines: `/nix/var/nix/profiles/system-NNN-link` (older generation), a
`git worktree` of some commit, another host's drvPath from the same checkout.

## Scope it to what the change can reach

| Change lives in | Diff |
|---|---|
| `p1/`, `p1g8/`, `nuc/` | that host |
| `laptop/` | p1 + p1g8 |
| `nixos-shared/`, `flake-base.nix`, an overlay | eval every importing host (~12 s each), build the one you can test |
| a package / script | the package alone: `nix eval --raw .#nixosConfigurations.p1g8.pkgs.xclip.drvPath`, `nix eval --raw .#myScripts.ts.drvPath` |

Cost ladder: eval a drvPath → drv-closure diff → build + `diff-closures` →
build all hosts. Most questions die at step two.

## Reading the output

- **Commit or `git add -A` first** — flake eval sees only tracked files, and a
  tree still being edited re-hashes on every eval: you diff a moving target.
- **The revision cone is expected noise.** `system.configurationRevision` means
  every commit changes the toplevel plus `nixos-version`, `system-path`, `etc`,
  restart-trigger units. The invariant is *which* drvs differ, never *that* the
  drvPath differs.
- **Changed drv ≠ changed content.** Anything embedding `system-path` (nuc's
  crontab, wrappers) re-hashes with identical logical content. Verify:
  ```bash
  txt() { nix derivation show "$1" | jq -r '.derivations[] | (.structuredAttrs // .env) | .text // .buildCommand'; }
  diff <(txt "$old_drv") <(txt "$new_drv")            # structuredAttrs vs env varies by nix version
  diff -r "$(nix-store -r "$old_drv")" "$(nix-store -r "$new_drv")"   # or just realize both
  ```
- **Same name, different path** = different build, not a different version.
  Tell: builder differs (`bash` vs `bash-static`). A path the cache doesn't
  have is a privately rebuilt cone, nearly always an accident —
  `nix path-info --store https://cache.nixos.org PATH` reports `is not valid`.
- **Why is this here / what costs space**: `nix why-depends A B` (`--derivation`
  for build-time), `nix path-info -rSh ./result | sort -k2 -h | tail`.

## Same diff, different expectation

| Question | Expected |
|---|---|
| Refactor / move / reformat — behaviour preserved? | revision cone only, zero package drvs |
| `nix flake update <input>` — what moved? | the input's dependents, nothing else |
| Removed a service or package | exactly its cone leaves |
| Does this shared-module edit reach nuc? | empty diff = no, definitively |
| Closure grew — why? | new paths, then `why-depends` the largest |
| Is `.#myScripts.<n>` what the host installs? | drvPath equality, no diff needed |

A cone-only diff is a stronger result than a successful build. Investigate
deviations; every surprise so far has been a real bug.
