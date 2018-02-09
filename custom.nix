# Useful nix hacks from:
# http://chriswarbo.net/projects/nixos/useful_hacks.html

with builtins;
with import <nixpkgs> {};
with lib;

rec {
  conditionalInclude = envVar: file:
  if getEnv envVar != ""
    then trace "Including: ${file} because ${envVar} is set" [ file ]
    else trace "Not including: ${file} because ${envVar} is not set" [];

  sanitiseName = stringAsChars (c: if elem c (lowerChars ++ upperChars)
                                      then c
                                      else "");

  fetchGitHashless = args: stdenv.lib.overrideDerivation
    (fetchgit (args // { sha256 = hashString "sha256" args.url; }))

    (old: {
      outputHash     = null;
      outputHashAlgo = null;
      outputHashMode = null;
      sha256         = null;
    });

  latestGitCommit = { url, ref ? "HEAD" }:
    runCommand "repo-${sanitiseName ref}-${sanitiseName url}"
      {
        # Avoids caching. This is a cheap operation and needs to be up-to-date
        version = toString currentTime;

        # Required for SSL
        GIT_SSL_CAINFO = "${cacert}/etc/ssl/certs/ca-bundle.crt";

        buildInputs = [ git gnused ];
      }
      ''
        REV=$(git ls-remote "${url}" "${ref}") || exit 1

        printf '"%s"' $(echo "$REV"        |
                        head -n1           |
                        sed -e 's/\s.*//g' ) > "$out"
      '';

  fetchLatestGit = { url, ref ? "HEAD" }@args:
    with { rev = import (latestGitCommit { inherit url ref; }); };
    fetchGitHashless (removeAttrs (args // { inherit rev; }) [ "ref" ]);
}
