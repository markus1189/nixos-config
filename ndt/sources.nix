# Pure loader for sources.json.
#
# This used to shell out to the `ndt` binary via import-from-derivation and
# defaulted to `<nixpkgs>` — neither works under pure flake evaluation, and
# the IFD forced a GHC build before anything could even evaluate.  Instead,
# parse sources.json directly with builtins fetchers:
#
#   - github            -> builtins.fetchTarball (checked against sha256)
#   - github+submodules -> builtins.fetchGit (rev-pinned; tarballs lack submodules)
#   - url               -> builtins.fetchurl (flat file, checked against sha256)
#
# Every attribute from sources.json (rev, date, sha256, ...) is preserved;
# `outPath` points at the fetched tree, so `${ndtSources.foo}` interpolation
# keeps working.  `ndt` itself is still used to *update* sources.json.
#
# Note: the github sha256 comes from nix-prefetch-git (NAR hash of the
# checkout), which matches the unpacked GitHub tarball except for repos using
# .gitattributes export tricks.  On a mismatch Nix prints the actual hash —
# fix it up in sources.json.
{ pkgs ? null # unused; kept so old `import ./sources.nix { inherit pkgs; }` call sites don't break
, sourcesFile ? ./sources.json
}:

let
  sources = builtins.fromJSON (builtins.readFile sourcesFile);

  fetch = name: spec:
    if spec.type == "github" then
      spec // {
        outPath =
          if spec.fetchSubmodules or false then
            builtins.fetchGit {
              url = spec.url;
              rev = spec.rev;
              ref = spec.branch;
              submodules = true;
              shallow = true;
            }
          else
            builtins.fetchTarball {
              name = spec.name or spec.repo;
              url = "https://github.com/${spec.owner}/${spec.repo}/archive/${spec.rev}.tar.gz";
              sha256 = spec.sha256;
            };
      }
    else if spec.type == "url" then
      spec // {
        outPath = builtins.fetchurl {
          name = spec.name or (baseNameOf spec.url);
          url = spec.url;
          sha256 = spec.sha256;
        };
      }
    else
      throw "ndt/sources.nix: unsupported source type '${spec.type}' for '${name}'";

in
builtins.mapAttrs fetch sources
