{ config, ... }:

{
  age = {
    secrets = {
      garminConnect = {
        file = ../secrets/garmin-connect.age;
        owner = config.my.userName;
      };
    };
  };
}
