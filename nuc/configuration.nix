# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  ndtSources = import ../ndt/sources.nix {};
  homeManager = "${ndtSources.home-manager.outPath}/nixos/default.nix";
in
{
  imports =
    [
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
      (import ../nixos-shared/home-manager/module.nix { homeNixFile = ./home.nix; })
      ./kodi.nix
      ./plex.nix
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

    timeServers = [ "0.nixos.pool.ntp.org" "1.nixos.pool.ntp.org" "2.nixos.pool.ntp.org" "3.nixos.pool.ntp.org" ];
  };

  time.timeZone = "Europe/Berlin";

  nixpkgs = {
    overlays = (import ../nixos-shared/shared-overlays.nix).overlays;
    config = {
      allowUnfree = true;
    };
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
    variables = {
      EDITOR = "${pkgs.vim}/bin/vim";
    };

    systemPackages = with pkgs; [
      bashmount
      coreutils
      feh
      jq
      lsof
      mplayer
      youtube-dl
      nixUnstable
      nix-index
      parallel
      pciutils
      pmutils
      psmisc
      pv
      rlwrap
      rsync
      stack
      tigervnc
      tree
      unrar
      unzip
      which
      xclip
      zathura
      zip
      zsh

      vim
      wget
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

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
  };

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
      extraConfig = "\Defaults: ${config.lib._custom_.userName} timestamp_timeout=30\n";
    };
  };

  system = {
    stateVersion = "19.03";
    autoUpgrade = {
      enable = true;
      dates = "04:21";
      flags = [ "-I" "nixos-config=/home/mediacenter/repos/nixos-config/nuc/configuration.nix" ];
    };
  };
}
