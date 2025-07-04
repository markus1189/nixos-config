{ config, pkgs, ... }:

let
  usrPkgs = pkgs.callPackage ../nixos-shared/packages/scripts { };
  custom = import ../nixos-shared/custom.nix;
  secrets = import ../nixos-shared/secrets.nix;
  ndtSources = import ../ndt/sources.nix { };
  homeManager = "${ndtSources.home-manager.outPath}/nixos/default.nix";
  myWallpaper =
    "${pkgs.nixos-artwork.wallpapers.nineish-dark-gray}/share/wallpapers/nineish-dark-gray-2020-07-02/contents/images/nix-wallpaper-nineish-dark-gray.png";
in rec {
  lib = {
    _custom_ = {
      userName = "markus";
      inherit ndtSources;
    };
  };

  imports = [
    (import ../nixos-shared/common-services.nix)
    ../nixos-shared/aws.nix
    ../nixos-shared/common-packages.nix
    ../nixos-shared/common-programs.nix
    ../nixos-shared/fasd.nix
    ../nixos-shared/fzf.nix
    ../nixos-shared/packages
    ../nixos-shared/packages/services.nix
    ../nixos-shared/rclone-mounts.nix
    ../nixos-shared/ripgrep.nix
    ../nixos-shared/ssh.nix
    (import ../nixos-shared/wireguard.nix "p1")
    ../nixos-shared/zsh.nix
    ./k8s.nix
    ./bluetooth.nix
    ./earlyoom.nix
    ./hosts.nix
    ./keybase.nix
    ./lastpass.nix
    ./low-battery.nix
    ./mopidy.nix
    ./programs.nix
    homeManager
    (import ../nixos-shared/home-manager/module.nix {
      homeNixFile = ./home.nix;
    })
    (import ./syncthing.nix config.lib._custom_.userName)
    ../nixos-shared/my-agenix.nix
    ./codecentric.nix
    ../nixos-shared/nix-ld.nix
    ../nixos-shared/rss-bridge.nix
    ../nixos-shared/reddit-top-rss.nix
    /etc/nixos/cachix.nix
  ];

  documentation = {
    enable = true;
    dev.enable = true;
  };

  nix = {
    gc = {
      automatic = false;
      dates = "12:30";
      options = "--delete-older-than 5d";
    };

    useSandbox = true;

    nixPath = with config.lib._custom_; [
      "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos"
      "nixos-config=/home/${userName}/repos/nixos-config/${name}/configuration.nix"
      "/nix/var/nix/profiles/per-user/root/channels"
    ];

    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  # boot = { extraModulePackages = with config.boot.kernelPackages; [ sysdig ]; };

  console = {
    font = "latarcyrheb-sun32";
    keyMap = "us";
  };

  i18n = { defaultLocale = "en_US.UTF-8"; };

  networking = {
    extraHosts = ''
      127.0.0.1 ${config.networking.hostName}
    '';

    firewall.allowedTCPPorts = [ ];

    timeServers = [
      "0.nixos.pool.ntp.org"
      "1.nixos.pool.ntp.org"
      "2.nixos.pool.ntp.org"
      "3.nixos.pool.ntp.org"
    ];

    supplicant = {
      "${config.lib._custom_.wirelessInterface}" = {
        configFile.path = "/etc/wpa_supplicant.conf";
        userControlled.enable = true;
      };
    };
  };

  time.timeZone = "Europe/Berlin";

  nixpkgs = {
    overlays = ((import ../nixos-shared/shared-overlays.nix).overlays ++ [
      (self: super: {
        darktable = builtins.trace "INFO: Using latest darktable via overlay"
          super.darktable.overrideAttrs (old: rec {
            name = "darktable-${self.ndtSources.darktable.rev}";
            version = self.ndtSources.darktable.rev;
            src = self.ndtSources.darktable.outPath;
            patches = [ ];
            dontVersionCheck = true;
            postPatch = ''
              patchShebangs tools/generate_styles_string.sh
            '';
          });
      })
    ]);

    config = { allowUnfree = true; };
  };

  services = {
    offlineimap.enable = false;

    upower.enable = false;

    tlp = {
      enable = false;
      settings = { USB_BLACKLIST = "046d:c52b"; };
    };

    x11vnc = {
      enable = true;
      auth = "/home/${config.lib._custom_.userName}/.Xauthority";
      password = secrets.x11vnc.password;
      viewonly = false;
      shared = true;
      autoStart = false;
    };

    avahi.enable = true;

    dbus.enable = true;

    physlock = { enable = true; };

    printing = {
      enable = true;
      drivers = [ pkgs.gutenprint pkgs.foo2zjs pkgs.hplipWithPlugin ];
    };

    tuptime = { enable = true; };

    xserver = {
      enable = true;

      displayManager = {
        autoLogin = {
          enable = true;
          user = config.lib._custom_.userName;
        };

        defaultSession = "none+xmonad";

        lightdm = { enable = true; };

        sessionCommands = ''
          ${usrPkgs.singlehead}/bin/singlehead
          ${pkgs.xorg.xrdb}/bin/xrdb /etc/X11/Xresources
          ${pkgs.xorg.xsetroot}/bin/xsetroot -cursor_name left_ptr
          ${pkgs.xorg.xset}/bin/xset r rate 250 30
          ${pkgs.feh}/bin/feh --no-fehbg --bg-fill ${myWallpaper} &
          ${pkgs.trayer}/bin/trayer --edge bottom --align right --SetDockType true --SetPartialStrut true --expand true --width 20 --transparent true --alpha 0 --tint 0x000000 --height 17.5 --monitor primary &
        '';
      };

      windowManager = {
        xmonad = {
          enable = true;
          enableContribAndExtras = true;
        };
      };

      synaptics = { enable = false; };

      libinput = {
        enable = true;
        touchpad = {
          tapping = false;
          disableWhileTyping = true;
          naturalScrolling = false;
        };
      };
    };

    clipmenu = { enable = true; };

    pipewire = {
      audio.enable = true;
      pulse.enable = true;
      extraConfig.pipewire = {
        "99-disable-bell" = {
          "context.properties" = { "module.x11.bell" = false; };
        };
      };
    };

   # mozillavpn.enable = true;
  };

  users.extraUsers.${config.lib._custom_.userName} = {
    isNormalUser = true;
    uid = 1000;
    group = "users";
    extraGroups = [
      "adbusers" # e.g. for scrcpy
      "wheel"
      "audio"
      "docker"
      "lp"
      "wireshark"
      "video"
      "dialout" # allow access to serial ports
    ];
    shell = "${pkgs.zsh}/bin/zsh";
    home = "/home/${config.lib._custom_.userName}";
    initialPassword = "markus"; # for qemu
  };

  users.extraGroups.vboxusers.members = [ "${config.lib._custom_.userName}" ];

  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    packages = with pkgs; [
      corefonts
      google-fonts
      inconsolata
      iosevka
      powerline-fonts
      source-code-pro
      source-sans-pro
      source-serif-pro
      ubuntu_font_family
      unifont
    ] ++ builtins.filter pkgs.lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts);

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
    powertop.enable = true;
  };

  hardware = {
    # pulseaudio = {
    #   enable = true;
    #   package = pkgs.pulseaudioFull;
    #   support32Bit = true;
    #   extraConfig = ''
    #     load-module module-switch-on-connect
    #   '';
    # };

    opengl.driSupport32Bit = true;

    uinput.enable = true; # For Multimedia buttons on QuietComfort
  };

  security = {
    rtkit = {
      enable = true;
    };

    sudo = {
      enable = true;
      extraConfig = ''
        Defaults:${config.lib._custom_.userName} timestamp_timeout=30
        Defaults insults
      '';
    };
  };

  virtualisation = {
    docker = {
      enable = true;
      extraOptions =
        "--bip='172.30.0.1/16'"; # Change to avoid conflicts in routing
    };

    podman = { enable = true; };
  };

  # virtualisation.virtualbox.host.enable = true;
  # virtualisation.virtualbox.host.package = let
  #   nixpkgs2311 = import ndtSources.nixpkgs-2311 {
  #     config = {
  #       allowUnfreePredicate = pkg:
  #         builtins.elem (pkgs.lib.getName pkg) [ "zoom" ];
  #     };
  #   };
  # in nixpkgs2311.virtualbox;

  programs = {
    light.enable = true;

    captive-browser = {
      enable = true;
      interface = config.lib._custom_.wirelessInterface;
    };

    zsh = let
      modifiedZbell = pkgs.writeText "modified-zbell.sh" ''
        #!/usr/bin/env zsh
        [[ -o interactive ]] || return

        # get $EPOCHSECONDS. builtins are faster than date(1)
        zmodload zsh/datetime || return

        autoload -Uz add-zsh-hook || return

        (( ''${+zbell_duration} )) || zbell_duration=15

        (( ''${+zbell_ignore} )) || zbell_ignore=($EDITOR $PAGER vim vi emacs less)

        zbell_timestamp=$EPOCHSECONDS

        # right before we begin to execute something, store the time it started at
        zbell_begin() {
          zbell_timestamp=$EPOCHSECONDS
          zbell_lastcmd=$1
        }

        # when it finishes, if it's been running longer than $zbell_duration,
        # and we dont have an ignored command in the line, then print a bell.
        zbell_end() {
                LAST_EC=$?
          ran_long=$(( $EPOCHSECONDS - $zbell_timestamp >= $zbell_duration ))

          has_ignored_cmd=0
          for cmd in ''${(s:;:)zbell_lastcmd//|/;}; do
            words=(''${(z)cmd})
            util=''${words[1]}
            if (( ''${zbell_ignore[(i)$util]} <= ''${#zbell_ignore} )); then
              has_ignored_cmd=1
              break
            fi
          done

          if (( ! $has_ignored_cmd )) && (( ran_long )); then
                        if [[ "$LAST_EC" == 0 ]]; then
                          ${pkgs.libnotify}/bin/notify-send -u low "Command finished [$LAST_EC]" "$zbell_lastcmd"
                        else
                          ${pkgs.libnotify}/bin/notify-send -u critical "Command failed [$LAST_EC]" "$zbell_lastcmd"
                        fi
            print -n "\a"
          fi
        }

        add-zsh-hook preexec zbell_begin
        add-zsh-hook precmd zbell_end
      '';
    in {
      interactiveShellInit = ''
        source ${modifiedZbell}
      '';
    };

    wireshark = {
      enable = true;
      package = pkgs.wireshark;
    };

    # obs-studio = {
    #   enable = true;
    #   enableVirtualCamera = true;
    #   plugins = [ pkgs.obs-studio-plugins.obs-backgroundremoval ];
    # };
  };

  environment = {
    variables = {
      EDITOR = "${pkgs.vim}/bin/vim";
      "_JAVA_AWT_WM_NONREPARENTING" = "1";
      LIBGL_DRI3_DISABLE = "1";
    };

    interactiveShellInit = ''
      mkcd() {
        mkdir -p $1 && cd $1
      }

      clone() {
          cd ~/repos/clones && git clone "$1" && cd "$(basename "$1" .git)"
      }

      callPackage() {
        nix-shell -p "with import <nixpkgs> {}; (callPackage ( import $1 ) {})";
      }
    '';

    shellAliases = (with pkgs; {
      "..." = "cd ../..";
      ".." = "cd ..";
      cdpr = ''
        if git rev-parse --show-toplevel &> /dev/null; then cd $(git rev-parse --show-toplevel); else echo "Not a git repository"; fi'';
      clipout = "${xclip}/bin/xclip -o -selection clipboard";
      clip = "${xclip}/bin/xclip -i -selection clipboard";
      ff = "${emacs}/bin/emacsclient -n -c";
      FF = "${emacs}/bin/emacsclient -n";
      magit = ''${emacs}/bin/emacsclient -n -c -e "(magit-status)"'';
      cdt = "cd $(${coreutils}/bin/mktemp -d)";
      pwdc = "pwd | clip";
      wpa_cli =
        "${wpa_supplicant}/bin/wpa_cli -i ${config.lib._custom_.wirelessInterface}";
    });

    etc = let
      youtube-downloader-config-shared = ''
        -o %(upload_date)s_%(uploader)s_%(title)s_%(id)s.%(ext)s
        --restrict-filenames
        --embed-subs
      '';
    in {
      "yt-dlp.conf".text = youtube-downloader-config-shared;

      "X11/Xresources".text = ''
        Xcursor.theme: Adwaita

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

        URxvt*iso14755: False

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
