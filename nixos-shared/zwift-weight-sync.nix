{ config, ... }:

{
  age = {
    secrets = {
      zwiftWeightSync = {
        file = ../secrets/zwift-weight-sync.age;
        owner = config.lib._custom_.userName;
      };
    };
  };
}
