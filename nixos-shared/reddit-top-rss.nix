{ config, pkgs, ... }:

let
  secrets = import ./secrets.nix;
in
{
  virtualisation.oci-containers.containers = {
    redditTopRss = {
      autoStart = true;
      image = "johnny5w/reddit-top-rss:1.2";
      ports = [ "9999:8080" ];
      environment = {
        REDDIT_USER = secrets.reddit.redditTopRss.user;
        REDDIT_CLIENT_ID = secrets.reddit.redditTopRss.clientId;
        REDDIT_CLIENT_SECRET = secrets.reddit.redditTopRss.clientSecret;
      };
    };
  };
}
