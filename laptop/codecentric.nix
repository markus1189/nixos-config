{ config, pkgs, ... }:

{
  age = {
    secrets = {
      ccWlanCertificate = {
        file = ../secrets/cc-wlan-certificate.age;
        name = "wifi/cc-wlan-certificate.crt";
        owner = config.my.userName;
      };
    };
  };
}
