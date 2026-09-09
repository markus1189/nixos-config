{ config, pkgs, ... }:

let
  userName = config.my.userName;
  curl = "${pkgs.curl}/bin/curl";
in
{
  services = {
    cron = {
      enable = true;
      systemCronJobs = [
        " */5         * * *   * ${userName} ${curl} -s https://hc-ping.com/6656b215-0e49-48ff-9af0-a79c64faab9f" # Dead-man's switch
      ];
    };
  };
}
