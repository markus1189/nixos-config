{ config, pkgs, ... }:

{
  virtualisation.oci-containers.containers = {
    redditTopRss = {
      autoStart = true;
      image = "johnny5w/reddit-top-rss:latest";
      ports = [ "9999:8080" ];
    };
  };
}
