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
    ../nixos-shared/sudo.nix
    ../nixos-shared/syncthing-base.nix
    ../nixos-shared/user.nix
    ../nixos-shared/zsh.nix
    # disko (module wired in flake.nix) provides the schema for ./disko.nix
    # and synthesises `fileSystems` / `swapDevices` at switch time. That is
    # what makes `nixos-generate-config --no-filesystems` safe here.
    ./disko.nix
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
    # Explicit, and not to be raised: SDDM autologins this (wheel) user into
    # Plasma, so a live sudo timestamp is all that physical access still has
    # to get past. Nightly upgrades run as root under systemd and never sudo.
    sudoTimeout = 5;
  };

  home-manager.users.${config.my.userName}.imports = [ ./home.nix ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernel.sysctl."kernel.sysrq" = 1;

  ## Memory #################################################################
  # 4 GB installed, 3.72 GiB usable (MemTotal 3901052 kB), no ECC. zram
  # carries the daily working set; the 8 GiB swapfile from ./disko.nix is an
  # OOM backstop only (hibernation is off, so it never needs to fit RAM).
  zramSwap.enable = true;
  # memoryPercent is left at the nixpkgs default of 50. p1g8.nix lowers it to
  # 25, but that exists because 50 % of 62 G was a 31 G sponge; 50 % of 3.7 G
  # is ~1.9 G, which is the entire point of zram on a machine this small.

  # Read-ahead of 2^3 = 8 pages per swap-in amortises seek latency. zram has
  # no seek, so that is eight decompressions to use one page; 0 is the
  # documented setting for RAM-backed swap.
  boot.kernel.sysctl."vm.page-cluster" = 0;

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

  system = {
    stateVersion = "19.03";
    # Rebuilds nightly from the latest commit on GitHub (repo is public) —
    # `nix flake update` + commit + push on a laptop is the whole pipeline,
    # no manual pull here. The module passes --refresh itself in flake mode
    # (no stale tarball cache); -L puts build logs in the journal. Still no
    # nightly lock updates as root on purpose (builds the committed
    # flake.lock).
    autoUpgrade = {
      # Careful when re-installing this host: the timer is Persistent=true, and
      # a freshly installed root has no stamp in /var/lib/systemd/timers, so it
      # fires on the *first boot* rather than at `dates` -- turn this off while
      # master still describes the old disk layout.
      enable = true;
      dates = "04:21";
      flake = "github:markus1189/nixos-config#nuc";
      flags = [ "-L" ];
    };
  };

  systemd.services = {
    # A failed nightly upgrade is otherwise silent, a hung one doubly so:
    # oneshot defaults to no timeout, so it blocks the timer without ever
    # failing. TimeoutStartSec, not RuntimeMaxSec (no effect on oneshot).
    nixos-upgrade = {
      onFailure = [ "notify-upgrade-failure.service" ];
      serviceConfig.TimeoutStartSec = "4h";
    };
    notify-upgrade-failure = {
      description = "telegram notification about failed nixos-upgrade";
      serviceConfig = {
        Type = "oneshot";
        User = config.my.userName;
        Group = "users";
        ExecStart = "${pkgs.notifySendTelegram}/bin/notifySendTelegram 'nuc: nightly nixos-upgrade failed'";
      };
    };

    # nofail in ./fileSystems.nix makes an absent disk silent; this is the signal.
    check-media-mounts = {
      description = "warn about unmounted /media disks";
      serviceConfig = {
        Type = "oneshot";
        User = config.my.userName;
        Group = "users";
      };
      script = ''
        missing=()
        for m in /media/backups /media/multimedia /media/multimedia2; do
          ${pkgs.util-linux}/bin/mountpoint -q "$m" || missing+=("$m")
        done
        if [ ''${#missing[@]} -gt 0 ]; then
          ${pkgs.notifySendTelegram}/bin/notifySendTelegram "nuc: not mounted: ''${missing[*]}"
        fi
      '';
      startAt = "*-*-* 07:00:00";
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

  # Kodi's only video source and three cron jobs in ./cron.nix live here; the
  # 31 G of old content stayed on /mnt/old, but the directory has to exist.
  systemd.tmpfiles.rules = [
    "d /home/${config.my.userName}/Downloads 0755 ${config.my.userName} users -"
  ];
}
