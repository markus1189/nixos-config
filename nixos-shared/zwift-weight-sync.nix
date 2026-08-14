{ config, ... }:

{
  age = {
    secrets = {
      zwiftWeightSync = {
        file = ../secrets/zwift-weight-sync.age;
        owner = config.my.userName;
      };
    };
  };
}
