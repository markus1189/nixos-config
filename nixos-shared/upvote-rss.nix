# nixos-shared/upvote-rss.nix
#
# Replaces reddit-top-rss: same author, abandoned upstream since 2023, its RSS
# output confirmed broken (HTTP 500) since 2026-08-18. Threshold semantics and
# query API are unchanged, so the feed URLs in emacs-config.el need no edits.
{ config, ... }:

{
  age.secrets.upvoteRss = {
    file = ../secrets/upvote-rss.env.age;
    name = "upvote-rss.env";
  };

  virtualisation.oci-containers.containers = {
    upvoteRss = {
      autoStart = true;
      # v1.8.1 (2026-03-13), digest-pinned: tags are mutable, digests are not.
      #
      # Bumped by hand, unlike rss-bridge, which rides a flake input. Upstream
      # publishes only :latest and semver tags -- no per-commit tag -- so a
      # revision from a flake input maps to no image, and there is nothing for
      # `nix flake update` to move. Bump by checking for a newer release:
      #   gh release view -R johnwarne/upvote-rss --json tagName
      # then resolve the tag once and record the digest it names:
      #   skopeo inspect --format '{{.Digest}}' \
      #     docker://ghcr.io/johnwarne/upvote-rss:<tag>
      image = "ghcr.io/johnwarne/upvote-rss@sha256:607853aabe62f1d84a9c8e86e7da1632c756229d84510f45fc4d2adf623d4ade";
      # Loopback only: unauthenticated, and it fetches arbitrary URLs on request.
      # Host port stays 9999 so elfeed is unaffected; container port is 80 (was 8080).
      ports = [ "127.0.0.1:9999:80" ];
      # Measured: cold feed build 17-34s, warm 3ms. The generated unit runs with
      # --rm, so without this the cache is destroyed on every rebuild.
      volumes = [ "upvote-rss-cache:/app/cache" ];
      environmentFiles = [ config.age.secrets.upvoteRss.path ];
    };
  };
}
