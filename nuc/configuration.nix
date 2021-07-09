# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  ndtSources = import ../ndt/sources.nix { };
  homeManager = "${ndtSources.home-manager.outPath}/nixos/default.nix";
in {
  imports = [
    (import ../nixos-shared/common-services.nix)
    (import ../nixos-shared/restic/systemd.nix "/media/backups/Photos/")
    (import ./cron.nix)
    ../nixos-shared/common-packages.nix
    ../nixos-shared/common-programs.nix
    ../nixos-shared/fasd.nix
    ../nixos-shared/fzf.nix
    ../nixos-shared/packages
    ../nixos-shared/packages/services.nix
    ../nixos-shared/restic/module.nix
    ../nixos-shared/ripgrep.nix
    ../nixos-shared/ssh.nix
    ../nixos-shared/zsh.nix
    ./fileSystems.nix
    ./hardware-configuration.nix
    homeManager
    (import ../nixos-shared/home-manager/module.nix {
      homeNixFile = ./home.nix;
    })
    ./kodi.nix
    ./adguard.nix
    (import ../nixos-shared/wireguard.nix "nuc")
  ];

  lib = {
    _custom_ = {
      wirelessInterface = "wlp58s0";
      name = "nuc";
      userName = "mediacenter";
    };
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelParams = [
    # https://bugzilla.redhat.com/show_bug.cgi?id=805285
    # fix for 'Queue <x> is active on fifo 1 and stuck for 10000 ms'
    "iwlwifi.wd_disable=1"
  ];

  networking = {
    hostName = "nuc";

    supplicant = {
      "${config.lib._custom_.wirelessInterface}" = {
        configFile.path = "/etc/wpa_supplicant.conf";
      };
    };

    extraHosts = ''
      127.0.0.1 ${config.networking.hostName}
    '';

    timeServers = [
      "0.nixos.pool.ntp.org"
      "1.nixos.pool.ntp.org"
      "2.nixos.pool.ntp.org"
      "3.nixos.pool.ntp.org"
    ];
  };

  time.timeZone = "Europe/Berlin";

  nixpkgs = {
    overlays = (import ../nixos-shared/shared-overlays.nix).overlays;
    config = { allowUnfree = true; };
  };

  nix = {
    gc = {
      automatic = true;
      dates = "03:15";
      options = "--delete-older-than 30d";
    };
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment = {
    variables = { EDITOR = "${pkgs.vim}/bin/vim"; };

    systemPackages = with pkgs; [
      bashmount
      coreutils
      feh
      jq
      lsof
      mplayer
      nix-index
      nixUnstable
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
      youtube-dl
      zathura
      zip
      zsh
    ];
  };

  # Open ports in the firewall.
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  services.syncthing = {
    enable = true;
    package = pkgs.syncthing;
    configDir = "/home/${config.lib._custom_.userName}/.config/syncthing";
    dataDir = "/home/${config.lib._custom_.userName}/Sync";
    openDefaultPorts = true;
    systemService = true;
    user = "${config.lib._custom_.userName}";
  };

  # Enable the X11 windowing system.
  services.xserver = { enable = true; };

  # Enable touchpad support.
  # services.xserver.libinput.enable = true;

  # Enable the KDE Desktop Environment.
  services.xserver.displayManager.sddm = {
    enable = true;
    autoLogin = {
      enable = true;
      user = "${config.lib._custom_.userName}";
      relogin = true;
    };
  };
  services.xserver.desktopManager.plasma5.enable = true;

  services.x11vnc = {
    enable = true;
    auth = "/home/${config.lib._custom_.userName}/.Xauthority";
    password = "worldbuilding2";
    shared = true;
    autoStart = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.${config.lib._custom_.userName} = {
    isNormalUser = true;
    group = "users";
    extraGroups = [ "wheel" "audio" "docker" "lp" ];
    shell = "${pkgs.zsh}/bin/zsh";
    home = "/home/${config.lib._custom_.userName}";
    uid = 1000;
  };

  security = {
    sudo = {
      enable = true;
      extraConfig = ''
        Defaults: ${config.lib._custom_.userName} timestamp_timeout=30
      '';
    };
  };

  system = {
    stateVersion = "19.03";
    autoUpgrade = {
      enable = true;
      dates = "04:21";
      flags = [
        "-I"
        "nixos-config=/home/mediacenter/repos/nixos-config/nuc/configuration.nix"
      ];
    };
  };

  systemd.services = {
    remind-personal-notifications = {
      description = "remind unit for personal notifications";
      serviceConfig = {
        User = config.lib._custom_.userName;
        Group = "users";
        ExecStart =
          "${pkgs.remind}/bin/remind -z -k'${pkgs.notifySendTelegram}/bin/notifySendTelegram %%s' /home/${config.lib._custom_.userName}/.reminders";
        Restart = "always";
      };
      wantedBy = [ "multi-user.target" ];
    };

    remind-home-notifications = {
      description = "remind unit for home notifications";
      serviceConfig = {
        User = config.lib._custom_.userName;
        Group = "users";
        ExecStart =
          "${pkgs.remind}/bin/remind -z -k'${pkgs.sendCurrentTemperature}/bin/sendCurrentTemperature' /home/${config.lib._custom_.userName}/home-notification-reminders";
        Restart = "always";
      };
      wantedBy = [ "multi-user.target" ];
    };
  };
}
