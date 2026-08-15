{ config, ... }:

# Central declarations for agenix runtime secrets consumed by scripts and
# services at fixed /run/agenix/<name> paths. Owner defaults to root; the
# user-owned ones are read by user-context scripts (shell, xmobar, user units).
let
  user = config.my.userName;
in
{
  age.secrets = {
    telegramEnv = {
      file = ../secrets/telegram.env.age;
      name = "telegram.env";
      owner = user;
    };

    toggl = {
      file = ../secrets/toggl.age;
      owner = user;
    };

    pushbullet = {
      file = ../secrets/pushbullet.age;
      owner = user;
    };

    raindrop = {
      file = ../secrets/raindrop.age;
      owner = user;
    };

    viessmannRefreshToken = {
      file = ../secrets/viessmann-refresh-token.age;
      name = "viessmann-refresh-token";
      owner = user;
    };

    authinfo = {
      file = ../secrets/authinfo.age;
      owner = user;
    };

    redditVisidata = {
      file = ../secrets/reddit-visidata.age;
      name = "reddit-visidata";
      owner = user;
    };
  };
}
