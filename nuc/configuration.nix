# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let 
  userName = "mediacenter";
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ../nixos-shared/packages
      ../nixos-shared/packages/services.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "nuc"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.wireless.userControlled.enable = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  nixpkgs = {
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
  environment.systemPackages = with pkgs; [
    bashmount
    coreutils
    feh
    firefoxWrapper
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

    gitFull
    (gitAndTools.git-extras)
    git-secret
    kodi
    vim
    wget 
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
    permitRootLogin = "prohibit-password";
    extraConfig = ''
      PermitEmptyPasswords no
      AllowUsers ${userName}
    '';
  };

  # Open ports in the firewall.
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ 
    42424 # kodi mediacenter
  ];

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us";
    xkbVariant = "altgr-intl";
    xkbOptions = "eurosign:e,caps:ctrl_modifier";
  };

  # Enable touchpad support.
  # services.xserver.libinput.enable = true;

  # Enable the KDE Desktop Environment.
  services.xserver.displayManager.sddm = {
    enable = true;
    autoLogin = {
      enable = true;
      user = "${userName}";
      relogin = true;
    };
  };
  services.xserver.desktopManager.plasma5.enable = true;

  services.x11vnc = {
    enable = true;
    auth = "/home/${userName}/.Xauthority";
    password = "worldbuilding2";
    shared = true;
    autoStart = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.${userName} = {
    isNormalUser = true;
    group = "users";
    extraGroups = [ "wheel" "audio" "docker" "lp" ];
    shell = "${pkgs.zsh}/bin/zsh";
    home = "/home/${userName}";
    uid = 1000;
  };

  security = {
    sudo = {
      enable = true;
      extraConfig = "\Defaults: ${userName} timestamp_timeout=30\n";
    };
  };

  system = {
    stateVersion = "18.09";
    autoUpgrade = {
      enable = true;
      dates = "04:21";
    };
  };

  fileSystems = {
    "multimedia1" = {
      mountPoint = "/media/multimedia";
      neededForBoot = false;
      device = "/dev/disk/by-uuid/C6B89CABB89C9B8D";
      fsType = "ntfs-3g";
      options = [ "defaults" "nls=utf8" "umask=000" "dmask=027" "uid=1000" "gid=100" "windows_names" ];
    };

    "multimedia2" = {
      mountPoint = "/media/multimedia2";
      neededForBoot = false;
      device = "/dev/disk/by-uuid/9E167A141679EE21";
      fsType = "ntfs-3g";
      options = [ "defaults" "nls=utf8" "umask=000" "dmask=027" "uid=1000" "gid=100" "windows_names" ];
    };

    "backups" = {
      mountPoint = "/media/backups";
      neededForBoot = false;
      device = "/dev/disk/by-uuid/AADEEA03DEE9C7A1";
      fsType = "ntfs-3g";
      options = [ "defaults" "nls=utf8" "umask=000" "dmask=027" "uid=1000" "gid=100" "windows_names" ];
    };
  };
}
