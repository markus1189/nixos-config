{ config, pkgs, ... }:

{
  age.secrets.redditTopRss = {
    file = ../secrets/reddit-top-rss.env.age;
    name = "reddit-top-rss.env";
  };

  virtualisation.oci-containers.containers = {
    redditTopRss = {
      autoStart = true;
      image = "johnny5w/reddit-top-rss:1.2";
      ports = [ "9999:8080" ];
      extraOptions = [ "--health-start-period=30s" ];
      environmentFiles = [ config.age.secrets.redditTopRss.path ];
    };
  };
}
