{ config, ... }:

{
  age = {
    secrets = {
      garminConnect = {
        file = ../secrets/garmin-connect.age;
        owner = config.lib._custom_.userName;
      };
    };
  };
}
