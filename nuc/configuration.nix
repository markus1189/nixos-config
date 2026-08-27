{
  config,
  pkgs,
  ...
}:

{
  imports = [
    ../nixos-shared/common-services.nix
    ../nixos-shared/restic/systemd.nix
    ./cron.nix
    ../nixos-shared/common-packages.nix
    ../nixos-shared/common-programs.nix
    ../nixos-shared/fasd.nix
    ../nixos-shared/fzf.nix
    ../nixos-shared/packages
    ../nixos-shared/packages/services.nix
    ../nixos-shared/runtime-secrets.nix
    ../nixos-shared/garmin-connect.nix
    ../nixos-shared/zwift-weight-sync.nix
    ../nixos-shared/rclone-mounts.nix
    ../nixos-shared/restic/module.nix
    ../nixos-shared/ripgrep.nix
    ../nixos-shared/ssh.nix
    ../nixos-shared/syncthing-base.nix
    ../nixos-shared/user.nix
    ../nixos-shared/zsh.nix
    ./fileSystems.nix
    ./hardware-configuration.nix
    ./kodi.nix
    ./atuin.nix
    ../nixos-shared/wireguard.nix
  ];

  my = {
    wirelessInterface = "wlp58s0";
    userName = "mediacenter";
    resticPhotoBackupDir = "/media/backups/Photos/";
  };

  home-manager.users.${config.my.userName}.imports = [ ./home.nix ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernel.sysctl."kernel.sysrq" = 1;

  networking = {
    hostName = "nuc";

    supplicant = {
      "${config.my.wirelessInterface}" = {
        configFile.path = "/etc/wpa_supplicant.conf";
        userControlled.enable = true;
      };
    };

    extraHosts = ''
      127.0.0.1 ${config.networking.hostName}
    '';
  };

  time.timeZone = "Europe/Berlin";

  nix = {
    gc = {
      automatic = true;
      dates = "03:15";
      options = "--delete-older-than 30d";
    };
  };

  environment = {
    variables = {
      EDITOR = "${pkgs.vim}/bin/vim";
    };

    systemPackages = with pkgs; [
      bashmount
      coreutils
      google-chrome
      feh
      jq
      lsof
      nixVersions.git
      parallel
      pciutils
      pmutils
      psmisc
      pv
      remind
      rlwrap
      rsync
      stack
      tigervnc
      tree
      unrar
      unzip
      vim
      wget
      which
      wyrd
      xclip
      zathura
      zip
      zsh
    ];
  };

  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ 4225 ];

  services.xserver = {
    enable = true;
  };

  services.displayManager = {
    autoLogin = {
      enable = true;
      user = "${config.my.userName}";
    };

    sddm = {
      enable = true;
      autoLogin = {
        relogin = true;
      };
    };
  };

  services.desktopManager.plasma6.enable = true;

  # User account skeleton comes from ../nixos-shared/user.nix

  security = {
    sudo = {
      enable = true;
      extraConfig = ''
        Defaults: ${config.my.userName} timestamp_timeout=30
      '';
    };
  };

  system = {
    stateVersion = "19.03";
    # Rebuilds nightly from the latest commit on GitHub (repo is public) —
    # `nix flake update` + commit + push on a laptop is the whole pipeline,
    # no manual pull here. The module passes --refresh itself in flake mode
    # (no stale tarball cache); -L puts build logs in the journal. Still no
    # nightly lock updates as root on purpose (builds the committed
    # flake.lock).
    autoUpgrade = {
      enable = true;
      dates = "04:21";
      flake = "github:markus1189/nixos-config#nuc";
      flags = [ "-L" ];
    };
  };

  systemd.services = {
    # A failed nightly upgrade is otherwise silent.
    nixos-upgrade.onFailure = [ "notify-upgrade-failure.service" ];
    notify-upgrade-failure = {
      description = "telegram notification about failed nixos-upgrade";
      serviceConfig = {
        Type = "oneshot";
        User = config.my.userName;
        Group = "users";
        ExecStart = "${pkgs.notifySendTelegram}/bin/notifySendTelegram 'nuc: nightly nixos-upgrade failed'";
      };
    };

    remind-personal-notifications = {
      description = "remind unit for personal notifications";
      serviceConfig = {
        User = config.my.userName;
        Group = "users";
        ExecStart = "${pkgs.remind}/bin/remind -z -k'${pkgs.notifySendTelegram}/bin/notifySendTelegram %%s' /home/${config.my.userName}/Syncthing/remind/reminders";
        Restart = "always";
      };
      wantedBy = [ "multi-user.target" ];
    };

    remind-home-notifications = {
      description = "remind unit for home notifications";
      serviceConfig = {
        User = config.my.userName;
        Group = "users";
        # homeWeatherReport = outside temperature (as before) plus the rain-radar loop.
        ExecStart = "${pkgs.remind}/bin/remind -z -k'${pkgs.homeWeatherReport}/bin/homeWeatherReport' /home/${config.my.userName}/Syncthing/remind/home-notification-reminders";
        Restart = "always";
      };
      wantedBy = [ "multi-user.target" ];
    };
  };
}
