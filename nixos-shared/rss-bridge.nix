{ config, pkgs, ... }:

{
  virtualisation.oci-containers.containers = {
    rssBridge = {
      autoStart = true;
      image = "rssbridge/rss-bridge";
      ports = [ "9998:80" ];
    };
  };
}
