{
  containers.gitlab-example = {
    autoStart = false;
    config = { config, pkgs, ...}:
    {
      services = {
        gitlab = {
          enable = true;
          initialRootPassword = "UseNixOS!";
          databasePassword = "test";
          secrets = {
            secret = "test-secret";
            otp = "test-otp";
            db = "test-db";
            jws = "test-jws";
          };
        };

        nginx = {
          enable = true;
          recommendedGzipSettings = true;
          recommendedOptimisation = true;
          recommendedProxySettings = true;
          recommendedTlsSettings = true;
          virtualHosts."git.example.com" = {
            enableACME = true;
            forceSSL = true;
            locations."/".proxyPass = "http://unix:/run/gitlab/gitlab-workhorse.socket";
          };
        };
      };
    };
  };
}
