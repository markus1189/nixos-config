let dummy = "just-a-dummy-value";
in {
  x11vnc = { password = dummy; };

  googlepw = dummy;

  restic = {
    b2bucket = {
      name = dummy;
      password = dummy;
      account-id = dummy;
      account-key = dummy;
    };
  };

  pushBulletToken = dummy;

  telegramBotToken = dummy;

  pocket = {
    access_token = dummy;
    consumer_key = dummy;
  };

  gasboiler = {
    username = dummy;
    password = dummy;
  };
}
