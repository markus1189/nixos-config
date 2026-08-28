{
  config,
  lib,
  pkgs,
  ...
}:

# botler: a Telegram bot that long-polls getUpdates and runs a whitelisted table
# of commands for a small allowlist of chat ids. The counterpart to the
# notifySend* family, which can only push on a schedule.
#
# Two things are deliberate and easy to get wrong later:
#
# - It uses a *second* bot token (secrets/botler.env.age), not telegram.env.
#   getUpdates is exclusive per token: two pollers both get HTTP 409, and
#   whichever polls first consumes the update. telegram.env's bot is already
#   read by the `telegram` agent skill's check_messages.py.
#
# - Exactly one host may enable this at a time, for the same reason.
let
  cfg = config.my.botler;

  # Passed as a store file rather than an ExecStart argument so systemd never
  # has to parse embedded JSON quoting.
  commandsFile = pkgs.writeText "botler-commands.json" (builtins.toJSON cfg.commands);

  allowFlags = lib.concatMapStringsSep " " (id: "--allow ${lib.escapeShellArg id}") cfg.allowedChatIds;
in
{
  options.my.botler = {
    enable = lib.mkEnableOption "botler, a Telegram command bot";

    allowedChatIds = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ "299952716" ];
      description = ''
        Chat ids allowed to issue commands; everything else is logged and dropped
        without a reply. The default is markus' own Telegram user id, which is
        also the chat id of the private chat with any bot. Not a secret: it is
        already in the clear in nixos-shared/packages/scripts/default.nix.
      '';
    };

    commands = lib.mkOption {
      default = { };
      description = ''
        Command table, keyed by the name typed after the slash. The handler is
        never given the bot token and never talks to Telegram itself -- it only
        produces output, which botler delivers.
      '';
      type = lib.types.attrsOf (
        lib.types.submodule {
          options = {
            help = lib.mkOption {
              type = lib.types.str;
              default = "";
              description = "One-line description, shown by /help.";
            };

            kind = lib.mkOption {
              type = lib.types.enum [
                "text"
                "animation"
              ];
              default = "text";
              description = ''
                "text": the handler's stdout is sent as a message.
                "animation": the handler is called with a destination .gif path
                as its only argument, and the file is uploaded via sendAnimation.
              '';
            };

            exec = lib.mkOption {
              type = lib.types.str;
              description = "Absolute path of the executable to run.";
            };

            caption = lib.mkOption {
              type = lib.types.str;
              default = "";
              description = ''Caption for the upload; "animation" only.'';
            };
          };
        }
      );
    };
  };

  config = lib.mkIf cfg.enable {
    age.secrets.botlerEnv = {
      file = ../secrets/botler.env.age;
      name = "botler.env";
      # systemd reads EnvironmentFile as root, so this is only so the unit can
      # be reproduced by hand for a test run.
      owner = config.my.userName;
    };

    systemd.services.botler = {
      description = "botler, a Telegram command bot";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        User = config.my.userName;
        Group = "users";
        ExecStart = "${pkgs.botler}/bin/botler --commands-file ${commandsFile} ${allowFlags}";
        EnvironmentFile = config.age.secrets.botlerEnv.path;
        # Units get no /etc/profile, so python's ssl module would fall back to
        # OpenSSL's compiled-in defaults. Pin the bundle, as the curl-based
        # senders do with --cacert.
        Environment = [ "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt" ];
        # /var/lib/botler, owned by User=, exported as $STATE_DIRECTORY: holds
        # the getUpdates offset so a restart does not replay handled commands.
        StateDirectory = "botler";
        Restart = "always";
        RestartSec = 10;
      };
    };
  };
}
