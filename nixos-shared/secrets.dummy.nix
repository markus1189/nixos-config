# Dummy stand-ins for every attribute the configuration reads from
# secrets.nix.  Used by ./load-secrets.nix whenever the real (git-secret
# revealed, gitignored) secrets.nix is absent — i.e. under pure flake
# evaluation of the git tree.  Keep this in sync with the attributes
# accessed across the repo (rg 'secrets\.' is your friend).
let
  dummy = "just-a-dummy-value";

  # Shaped like a wg-quick interface so the module still evaluates.
  # autostart = false so a dummy-built system doesn't try to bring up a
  # VPN with an all-zero key.
  dummyWireguardInterface = {
    autostart = false;
    address = [ "10.0.0.2/32" ];
    privateKey = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
    peers = [
      {
        publicKey = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
        allowedIPs = [ "0.0.0.0/0" ];
        endpoint = "192.0.2.1:51820";
      }
    ];
  };
in {
  garminConnect = { password = dummy; };

  x11vnc = { password = dummy; };

  googlepw = dummy;

  gptel = {
    anthropic = dummy;
    deepseek = dummy;
    gemini = dummy;
    openai = dummy;
    openrouter = dummy;
    perplexity = dummy;
    xai = dummy;
  };

  raindrop = { test_token = dummy; };

  reddit = {
    redditTopRss = {
      user = dummy;
      clientId = dummy;
      clientSecret = dummy;
    };
    visidata = dummy;
  };

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

  shellyWebUI = {
    username = dummy;
    password = dummy;
  };

  pocket = {
    access_token = dummy;
    consumer_key = dummy;
  };

  toggl = dummy;

  viessmannRefreshToken = dummy;

  wireguard = {
    mozilla = {
      p1 = { wg-nyc = dummyWireguardInterface; };
      p1g8 = { wg-nyc = dummyWireguardInterface; };
      nuc = { wg-nyc = dummyWireguardInterface; };
    };
  };

  gasboiler = {
    username = dummy;
    password = dummy;
  };
}
