# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  codecentricWallpaper = pkgs.fetchurl {
    url = "https://www.dropbox.com/s/hoy6xdgnudwy64g/codecentric.jpg?dl=1";
    sha256 = "0z8ia5gv1r3brr7a2cc3sv0ffi0vqdmwj85b404702v7sb7f2d1k";
  };
  userName = "markus";
  usrPkgs = import ./scripts/scripts.nix { inherit pkgs; };
  custom = import ./custom.nix;
in
rec {
  imports =
    [
      ./bluetooth.nix
      ./fasd.nix
      ./fzf.nix
      ./hardware-configuration.nix
      ./keybase.nix
      ./lastpass.nix
      ./packages.nix
      ./scripts/module.nix
      ./ssh.nix
      ./xps.nix
      ./zsh.nix
      ./packages
      ./packages/services.nix
      ./containers
      ./hosts.nix
      ./contextual/codecentric.nix
    ] ++ custom.conditionalInclude "NIX_AAREAL" ./contextual/aareal.nix;

  nix = {
    gc = {
      automatic = false;
      dates = "12:30";
      options = "--delete-older-than 5d";
    };

    useSandbox = true;
  };

  boot = {
    extraModulePackages = with config.boot.kernelPackages; [ sysdig ];
  };

  i18n = {
    consoleFont = "latarcyrheb-sun32";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  networking = {
    wireless = {
      enable = true;
      userControlled.enable = true;
    };

    extraHosts = ''
      127.0.0.1 ${config.networking.hostName}
    '';

    timeServers = [ "0.nixos.pool.ntp.org" "1.nixos.pool.ntp.org" "2.nixos.pool.ntp.org" "3.nixos.pool.ntp.org" ];
  };

  time.timeZone = "Europe/Berlin";

  nixpkgs = {
    config = {

      allowUnfree = true;

      firefox = {
        enableGoogleTalkPlugin = true;
        enableOfficialBranding = true;
      };

      chromium = {
        enableWideVine = false;
        enablePepperFlash = true;
      };
    };
  };

  services = {
    actkbd = {
      bindings = [
        {
          keys = [171];
          events = ["key"];
          command = "${pkgs.playerctl}/bin/playerctl next";
        }
        {
          keys = [172];
          events = ["key"];
          command = "${pkgs.playerctl}/bin/playerctl previous";
        }
        {
          keys = [173];
          events = ["key"];
          command = "${pkgs.playerctl}/bin/playerctl play-pause";
        }
      ];
    };

    avahi.enable = true;

    atd.enable = true;

    cron = {
      enable = true;
      mailto = userName;
    };

    dbus.enable = true;

    nixosManual = {
      enable = true;
      showManual = true;
      ttyNumber = 8;
    };

    mpd = {
      enable = false;
      musicDirectory = "/var/lib/mpd/music";
      extraConfig = ''
        # log_level "verbose"

        audio_output {
          type "alsa"
          name "My ALSA Device"
          device "front:CARD=PCH,DEV=0"
          format "44100:16:2"
          mixer_type "software"
        }

        audio_output {
          type "pulse"
          name "mpd pulse audio"
          mixer_type "software"
        }
      '';
    };

    udisks2.enable = true;

    physlock = {
      enable = true;
    };

    # printing = {
    #   enable = true;
    #   drivers = [ pkgs.gutenprint pkgs.foo2zjs ];
    # };

    unclutter.enable = true;

    locate = {
      enable = true;
      interval = "hourly";
      localuser = userName;
    };

    sysstat = {
      enable = true;
    };

    xserver = {
      enable = true;

      layout = "us";

      xkbVariant = "altgr-intl";
      xkbOptions = "eurosign:e,caps:ctrl_modifier";

      displayManager = {
        slim = {
          enable = true;
          defaultUser = userName;
          autoLogin = false;
        };

        sessionCommands = ''
          ${usrPkgs.singlehead}/bin/singlehead
          ${pkgs.xorg.xrdb}/bin/xrdb /etc/X11/Xresources
          ${pkgs.xlibs.xsetroot}/bin/xsetroot -cursor_name left_ptr
          ${pkgs.feh}/bin/feh --no-fehbg --bg-tile ${codecentricWallpaper} &
          ${pkgs.trayer}/bin/trayer --edge bottom --align right --SetDockType true --SetPartialStrut true --expand true --width 20 --transparent true --alpha 0 --tint 0x000000 --height 17.5 --monitor primary &
          ${pkgs.parcellite}/bin/parcellite &
        '';
      };

      desktopManager.default = "none";

      windowManager = {
        default = "xmonad";
        xmonad = {
          enable = true;
          enableContribAndExtras = true;
        };
      };
    };

    acpid = {
      enable = true;
    };
  };
      ".gitconfig" = pkgs.myConfigFiles.gitconfig;
      ".gitconfig" = pkgs.myConfigFiles.gitconfig;
      ".gitconfig" = pkgs.myConfigFiles.gitconfig;

  users.extraUsers.${userName} = {
    isNormalUser = true;
    uid = 1000;
    group = "users";
    extraGroups = [ "wheel" "audio" "docker" "lp" "wireshark" ];
    shell = "${pkgs.zsh}/bin/zsh";
    home = "/home/${userName}";
    initialPassword = "markus"; # for qemu
    symlinks = {
      ".xmonad/xmonad.hs" = pkgs.myConfigFiles.xmonad;
    };
  };

  users.extraGroups.vboxusers.members = [ "${userName}" ];

  system.stateVersion = "17.03";

  programs = {
    bash = {
      enableCompletion = true;
    };

    java = {
      enable = true;
      package = pkgs.oraclejdk;
    };
  };

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      corefonts
      google-fonts
      inconsolata
      powerline-fonts
      source-code-pro
      source-sans-pro
      source-serif-pro
      ubuntu_font_family
      unifont
    ];

    fontconfig = {
      antialias = true;
      defaultFonts = {
        monospace = [ "Source Code Pro" ];
        serif = [ "Source Serif Pro" ];
      };
    };
  };

  powerManagement = {
    enable = true;
  };

  hardware = {
    pulseaudio = {
      enable = true;
      support32Bit = true;
      extraConfig = ''
        load-module module-switch-on-connect
      '';
    };

    opengl.driSupport32Bit = true;
  };

  sound = {
    enable = true;
    mediaKeys.enable = false;
  };

  security = {
    sudo = {
      enable = true;
      extraConfig = "\nDefaults: ${userName} timestamp_timeout=30\n";
    };
  };

  virtualisation.docker.enable = true;

  virtualisation.virtualbox.host.enable = true;

  environment = {
    variables = {
      EDITOR = "${pkgs.vim}/bin/vim";
    };

    interactiveShellInit = ''
      mkcd() {
        mkdir -p $1 && cd $1
      }
    '';

    shellAliases = (with pkgs; {
      "..." = "cd ../..";
      ".." = "cd ..";
      clipout = "${xclip}/bin/xclip -o -selection clipboard";
      clip = "${xclip}/bin/xclip -i -selection clipboard";
      ff = "${emacs}/bin/emacsclient -n -c";
      ll = "${exa}/bin/exa -labgSh --git";
    });

    etc = {
      "youtube-dl.conf".text = ''
        -o %(upload_date)s_%(uploader)s_%(title)s_%(id)s.%(ext)s
        --restrict-filenames
      '';

      "X11/Xresources".text = ''
        URxvt*font: xft:Source Code Pro:size=11:antialias=true:hintingt=true,xft:Inconsolata-g for Powerline:size=11,xft:Code2000:antialias=false
        URxvt*cursorColor: #Ffe7ba
        URxvt*background:  #000000
        URxvt*foreground:  #f1f1f1
        URxvt*color0:      #363636
        URxvt*color1:      #Ee4000
        URxvt*color2:      #aece92
        URxvt*color3:      #Ffd700
        URxvt*color4:      #4f94cd
        URxvt*color5:      #963c59
        URxvt*color6:      #7ccd7c
        URxvt*color7:      #bebebe
        URxvt*color8:      #666666
        URxvt*color9:      #cf6171
        URxvt*color10:     #00fa9a
        URxvt*color11:     #Eec900
        URxvt*color12:     #E9967a
        URxvt*color13:     #Ffa500
        URxvt*color14:     #00ffff
        URxvt*color15:     #ffffff
        URxvt*underlineColor: #bebebe

        URxvt.urgentOnBell: true
        URxvt*transparent: true
        URxvt*saveLines: 3141592
        URxvt*shading: 15

        URxvt*termName: rxvt
        URxvt*scrollBar_right: false
        URxvt*scrollBar: false

        URxvt.perl-ext-common: default,url-select,font-size,clipboard,color-themes

        URxvt.keysym.M-u: perl:url-select:select_next
        URxvt.url-select.launcher: firefox
        URxvt.url-select.underline: false

        URxvt.resize-font.smaller: C-Down
        URxvt.resize-font.bigger: C-Up

        URxvt.keysym.C-plus: font-size:increase
        URxvt.keysym.C-equal: font-size:reset
        URxvt.keysym.C-minus: font-size:decrease

        URxvt.clipboard.autocopy: true
        URxvt.keysym.M-c: perl:clipboard:copy
        URxvt.keysym.M-v: perl:clipboard:paste
        URxvt.keysym.M-C-v: perl:clipboard:paste_escaped
        URxvt.clipboard.copycmd:  xclip -i -selection clipboard
        URxvt.clipboard.pastecmd: xclip -o -selection clipboard

        ! ------------------------------------------------------------------------------
        ! ROFI Color theme
        ! ------------------------------------------------------------------------------
        rofi.color-enabled: true
        rofi.color-window: #393939, #393939, #f3843d
        rofi.color-normal: #393939, #ffffff, #393939, #f3843d, #000000
        rofi.color-active: #393939, #f3843d, #393939, #f3843d, #000000
        rofi.color-urgent: #393939, #f3843d, #393939, #f3843d, #ffc39c
      '';

    };
  };
}
