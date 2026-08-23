{ inputs, ... }:
let
  # Upstream's CI publishes one image tag per commit on master, named
  # sha-<first 7 of the rev> (1714 of them as of 2026-08-23), alongside the
  # mutable :latest. So the flake input pins the image: `nix flake update
  # rss-bridge-src` moves the rev, the rev moves the tag, and the tag is
  # immutable in practice because it names exactly one commit's build.
  #
  # The one failure mode: the input can lock a commit whose image build has
  # not finished (or has failed), and podman then errors with "manifest
  # unknown". Check before switching:
  #   skopeo inspect --raw docker://docker.io/rssbridge/rss-bridge:sha-<rev>
  # and if it is missing, `nix flake lock --override-input rss-bridge-src
  # github:RSS-Bridge/rss-bridge/<older-rev>`.
  #
  # docker.io/ is not decoration: /etc/containers/registries.conf defines no
  # unqualified-search-registries, so a short name resolves only by accident
  # of a matching image already being in local storage.
  imageTag = "sha-${inputs.rss-bridge-src.shortRev}";
in
{
  virtualisation.oci-containers.containers = {
    rssBridge = {
      autoStart = true;
      image = "docker.io/rssbridge/rss-bridge:${imageTag}";
      ports = [ "127.0.0.1:9998:80" ];
    };
  };
}
