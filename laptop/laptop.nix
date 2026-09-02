{
  config,
  pkgs,
  inputs,
  ...
}:

{
  my.userName = "markus";

  home-manager.users.${config.my.userName}.imports = [ ./home.nix ];

  imports = [
    ../nixos-shared/common-services.nix
    ../nixos-shared/common-packages.nix
    ../nixos-shared/common-programs.nix
    ../nixos-shared/fasd.nix
    ../nixos-shared/fzf.nix
    ../nixos-shared/packages
    ../nixos-shared/packages/kanata/service.nix
    ../nixos-shared/packages/services.nix
    ../nixos-shared/rclone-mounts.nix
    ../nixos-shared/ripgrep.nix
    ../nixos-shared/ssh.nix
    ../nixos-shared/sudo.nix
    ../nixos-shared/user.nix
    ../nixos-shared/wireguard.nix
    ../nixos-shared/zsh.nix
    ./bluetooth.nix
    ./oom.nix
    ./hosts.nix
    ./lastpass.nix
    ./low-battery.nix
    ./programs.nix
    ../nixos-shared/syncthing-base.nix
    ../nixos-shared/runtime-secrets.nix
    ./codecentric.nix
    ../nixos-shared/nix-ld.nix
    ../nixos-shared/rss-bridge.nix
    ../nixos-shared/upvote-rss.nix
    ../nixos-shared/cachix.nix
    ../nixos-shared/botler.nix
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

    settings.sandbox = true;
  };

  # Shared across both ThinkPad P1 hosts (was verbatim in p1.nix and p1g8.nix)
  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    kernelPackages = pkgs.linuxPackages_latest;

    # Manual escape hatch from a memory stall. The default 16 is
    # SYSRQ_ENABLE_SYNC alone (include/linux/sysrq.h); `f`
    # (memory-full-oom-kill) sits behind SYSRQ_ENABLE_SIGNAL = 0x40, so
    # Alt+SysRq+f was answered with "This sysrq operation is disabled".
    # 1 means "enable all functions" -- sysrq.c:86 short-circuits the mask
    # on `sysrq_enabled == 1`. It also arms `c` (deliberate panic); use 252
    # instead if you want everything except that.
    #
    # Useful once live: f = OOM-kill the largest consumer, w = dump blocked
    # tasks, m = dump memory, R-E-I-S-U-B = clean-ish reboot. On a ThinkPad
    # SysRq is Fn+PrtSc -- check that now, not mid-stall.
    kernel.sysctl."kernel.sysrq" = 1;
  };

  console = {
    font = "latarcyrheb-sun32";
    keyMap = "us";
  };

  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  networking = {
    extraHosts = ''
      127.0.0.1 ${config.networking.hostName}
    '';

    firewall.allowedTCPPorts = [ ];

    wireless = {
      enable = true;
      interfaces = [ config.my.wirelessInterface ];
      userControlled = true;
      # Networks live in /etc/wpa_supplicant.conf (managed manually).
      # Remove ctrl_interface and update_config lines from that file —
      # the module provides them via /etc/wpa_supplicant/nixos.conf.
      extraConfigFiles = [ "/etc/wpa_supplicant.conf" ];
      # /etc/wpa_supplicant.conf must be readable by wpa_supplicant group:
      # sudo chgrp wpa_supplicant /etc/wpa_supplicant.conf && sudo chmod 640 /etc/wpa_supplicant.conf
    };
  };

  time.timeZone = "Europe/Berlin";

  nixpkgs.overlays = [
    (_: super: {
      darktable =
        builtins.trace "INFO: Using latest darktable via overlay" super.darktable.overrideAttrs
          (old: rec {
            name = "darktable-${inputs.darktable.rev}";
            version = inputs.darktable.rev;
            src = inputs.darktable;
            patches = [ ];
            dontVersionCheck = true;
            buildInputs = (old.buildInputs or [ ]) ++ [ super.potrace ];
            postPatch = ''
              patchShebangs tools/generate_styles_string.sh
            '';
          });
    })
  ];

  services = {
    offlineimap.enable = false;

    # required for suspend on low battery
    upower.enable = true;

    avahi.enable = true;

    dbus.enable = true;

    fstrim.enable = true;

    physlock = {
      enable = true;
    };

    printing = {
      enable = true;
      drivers = [
        pkgs.gutenprint
        pkgs.foo2zjs
        pkgs.hplipWithPlugin
      ];
    };

    tuptime = {
      enable = true;
    };

    xserver = {
      enable = true;

      displayManager = {
        lightdm = {
          enable = true;
        };

        sessionCommands = ''
          ${pkgs.xrdb}/bin/xrdb /etc/X11/Xresources
          ${pkgs.xsetroot}/bin/xsetroot -cursor_name left_ptr
          ${pkgs.xset}/bin/xset r rate 250 30
          ${pkgs.feh}/bin/feh --no-fehbg --bg-fill ${pkgs.markus-wallpapers.orange-cube-left} ${pkgs.markus-wallpapers.orange-cube-right} &
          ${pkgs.trayer}/bin/trayer --edge bottom --align right --SetDockType true --SetPartialStrut true --expand true --width 20 --transparent true --alpha 0 --tint 0x000000 --height 17.5 --monitor primary &
        '';
      };

      windowManager = {
        xmonad = {
          enable = true;
          enableContribAndExtras = true;
        };
      };

      # libinput configuration moved to services.libinput
    };

    # clipmenu replaced by home-manager services.clipcat (laptop/home.nix)

    # Tapping / click method are per-host: the old P1's pad misbehaved with
    # tapping (p1/p1.nix keeps it off), the P1 Gen 8 runs tapping +
    # clickfinger (p1g8/p1g8.nix).
    libinput = {
      enable = true;
      touchpad = {
        disableWhileTyping = true;
        naturalScrolling = false;
      };
    };

    displayManager = {
      autoLogin = {
        enable = true;
        user = config.my.userName;
      };

      defaultSession = "none+xmonad";
    };

    pipewire = {
      audio.enable = true;
      pulse.enable = true;
      extraConfig.pipewire = {
        "99-disable-bell" = {
          "context.properties" = {
            "module.x11.bell" = false;
          };
        };
      };
    };
  };

  # Skeleton (uid, group, shell, home, base groups) in ../nixos-shared/user.nix
  users.extraUsers.${config.my.userName} = {
    extraGroups = [
      "adbusers" # e.g. for scrcpy
      "wireshark"
      "video"
      "dialout" # allow access to serial ports
      "wpa_supplicant" # wpa_cli access
    ];
    initialPassword = "markus"; # for qemu
  };

  users.extraGroups.vboxusers.members = [ "${config.my.userName}" ];

  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    packages =
      with pkgs;
      [
        atkinson-hyperlegible-next
        corefonts
        google-fonts
        inconsolata
        iosevka
        powerline-fonts
        source-code-pro
        source-sans-pro
        source-serif-pro
        ubuntu-classic
        unifont
      ]
      ++ builtins.filter pkgs.lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts);

    fontconfig = {
      antialias = true;
      defaultFonts = {
        monospace = [
          "Source Code Pro"
          "Symbols Nerd Font Mono"
        ];
        sansSerif = [
          "Source Sans 3"
          "Symbols Nerd Font"
        ];
        serif = [
          "Source Serif 4"
          "Symbols Nerd Font"
        ];
      };
    };
  };

  powerManagement = {
    enable = true;
    powertop.enable = true;
  };

  hardware = {
    graphics.enable32Bit = true;
    graphics.extraPackages = with pkgs; [
      intel-compute-runtime
      intel-media-driver # iHD VAAPI driver — hw-decode on the Intel iGPU
    ];

    trackpoint = {
      device = "TPPS/2 Elan TrackPoint";
      enable = true;
      sensitivity = 112;
      speed = 97;
    };

    uinput.enable = true; # For Multimedia buttons on QuietComfort
  };

  security = {
    rtkit = {
      enable = true;
    };

    # Policy (execWheelOnly, timeout, lecture) is shared, see
    # ../nixos-shared/sudo.nix. Only the insults are laptop-local: they need
    # the custom build, and `Defaults insults` is inert without it.
    sudo = {
      package = pkgs.callPackage ./sudo-custom.nix { };
      extraConfig = "Defaults insults";
    };
  };

  virtualisation = {
    docker = {
      enable = true;
      extraOptions = "--bip='172.30.0.1/16'"; # Change to avoid conflicts in routing

      daemon = {
        settings = {
          default-ulimits = {
            nofile = {
              Name = "nofile";
              Hard = 8192; # MongoDB is not happy with lower limits
              Soft = 8192;
            };
          };
        };
      };
    };

    podman = {
      enable = true;
    };
  };

  programs = {

    captive-browser = {
      enable = true;
      interface = config.my.wirelessInterface;
    };

    zsh =
      let
        modifiedZbell = pkgs.writeText "modified-zbell.sh" ''
          #!/usr/bin/env zsh
          [[ -o interactive ]] || return

          # get $EPOCHSECONDS. builtins are faster than date(1)
          zmodload zsh/datetime || return

          autoload -Uz add-zsh-hook || return

          (( ''${+zbell_duration} )) || zbell_duration=15

          (( ''${+zbell_ignore} )) || zbell_ignore=($EDITOR $PAGER vim vi emacs less zathura sioyek evince koreader okular foliate imv mpv)

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

            # precmd fires on a bare Enter but preexec does not, so without this
            # guard zbell_lastcmd survives and every empty Enter after an idle
            # stretch re-notifies the last command (critical, if it failed).
            [[ -n $zbell_lastcmd ]] || return
            local ran=$zbell_lastcmd
            unset zbell_lastcmd

            ran_long=$(( $EPOCHSECONDS - $zbell_timestamp >= $zbell_duration ))

            # 130 = SIGINT (Ctrl-C), 148 = SIGTSTP (Ctrl-Z). Both mean you were
            # at the keyboard when it ended, so you already know. Codes verified
            # live against zsh on Linux, 2026-08-24.
            case $LAST_EC in
              130|148) return ;;
            esac

            has_ignored_cmd=0
            # Match any word of each segment, not just the first: `sudo
            # zathura`, `env X=1 sioyek` and wrappers all suppress the bell
            # without the wrapper itself being in the list.
            for seg in ''${(s:;:)ran//|/;}; do
              words=(''${(z)seg})
              for util in ''${words[@]}; do
                if (( ''${zbell_ignore[(i)$util]} <= ''${#zbell_ignore} )); then
                  has_ignored_cmd=1
                  break 2
                fi
              done
            done

            if (( ! $has_ignored_cmd )) && (( ran_long )); then
                          if [[ "$LAST_EC" == 0 ]]; then
                            ${pkgs.libnotify}/bin/notify-send -u low "Command finished [$LAST_EC]" "$ran"
                          else
                            ${pkgs.libnotify}/bin/notify-send -u critical "Command failed [$LAST_EC]" "$ran"
                          fi
              print -n "\a"
            fi
          }

          add-zsh-hook preexec zbell_begin
          add-zsh-hook precmd zbell_end
        '';
      in
      {
        interactiveShellInit = ''
          source ${modifiedZbell}
        '';
      };

    wireshark = {
      enable = true;
      package = pkgs.wireshark;
    };

    sysdig = {
      enable = true;
    };
  };

  environment = {
    variables = {
      EDITOR = "${pkgs.vim}/bin/vim";
      VISUAL = "emacsclient -c -a vim";
      "_JAVA_AWT_WM_NONREPARENTING" = "1";
    };

    interactiveShellInit = ''
      mkcd() {
        mkdir -p $1 && cd $1
      }

      clone() {
          cd ~/repos/clones && git clone "$1" && cd "$(basename "$1" .git)"
      }

      callPackage() {
        nix shell --impure --expr "with import (builtins.getFlake \"nixpkgs\") {}; callPackage (import $1) {}"
      }

      pwdc() {
        if [ $# -eq 0 ]; then
          pwd | ${pkgs.xclip}/bin/xclip -i -selection clipboard
        else
          realpath "$1" | ${pkgs.xclip}/bin/xclip -i -selection clipboard
        fi
      }
    '';

    shellAliases = with pkgs; {
      "..." = "cd ../..";
      ".." = "cd ..";
      cdpr = ''if git rev-parse --show-toplevel &> /dev/null; then cd $(git rev-parse --show-toplevel); else echo "Not a git repository"; fi'';
      clipout = "${xclip}/bin/xclip -o -selection clipboard";
      clip = "${xclip}/bin/xclip -i -selection clipboard";
      ff = "${emacs}/bin/emacsclient -n -c";
      FF = "${emacs}/bin/emacsclient -n";
      magit = ''${emacs}/bin/emacsclient -n -c -e "(magit-status)"'';
      wpa_cli = "${wpa_supplicant}/bin/wpa_cli -i ${config.my.wirelessInterface} -p /run/wpa_supplicant/control";
    };

    etc =
      let
        youtube-downloader-config-shared = ''
          # ---- Filenames / output ----
          #  title/uploader truncated by BYTES: no auto-clamping exists, and a long
          #  title would blow ext4's 255-byte limit. Trimming the fields (not
          #  --trim-filenames) keeps the trailing _%(id)s, the only stable key.
          -o %(upload_date)s_%(uploader).40B_%(title).120B_%(id)s.%(ext)s
          --restrict-filenames
          --output-na-placeholder ""            # empty, not literal "NA", for missing fields

          # ---- Container / quality ----
          -S lang,res:1080,fps,vcodec,acodec    # lang first: original audio over YT auto-dubs
          --merge-output-format mkv             # chosen when a merge happens...
          --remux-video mkv                     # ...and when it doesn't; mkv either way
          #  (leave -f at its default bv*+ba/b; -S above shapes quality)

          # ---- Embed everything into the one file (ffmpeg is bundled) ----
          --embed-metadata                      # title/uploader/description into tags
          --embed-chapters                      # redundant with --embed-metadata; kept as intent
          --embed-thumbnail                     # cover art (mkv = clean; mp4 would need AtomicParsley)
          --convert-thumbnails jpg              # mkv attaches as-is, and YT serves webp, which ~nothing renders
          --embed-info-json                     # writes a temp, attaches it, deletes it: no stray file
          --embed-subs
          --write-auto-subs                     # ASR captions; most videos have no manual subs at all
          --sub-langs en.*,de.*                 # makes --embed-subs actually do something
          --convert-subs srt                    # normalize so embedding always succeeds

          # ---- Robustness / speed ----
          --concurrent-fragments 8              # parallel DASH/HLS fragments = big real speedup
          --retries infinite
          --fragment-retries infinite
          --retry-sleep exp=1:120               # without these, "infinite" is a zero-delay hot loop
          --retry-sleep fragment:exp=1:60
          --throttled-rate 100K                 # re-extract & retry if a stream drops below 100 KiB/s

          # ---- SponsorBlock (non-destructive) ----
          #  Marks are merged INTO the chapter list and win every overlap, so keep
          #  the set narrow. Measured on one 64-min talk with 11 real chapters:
          #  "all" produced 67 chapters, 28 of the 32 marks being filler. Dropping
          #  filler is what saves the table; "chapter" is SponsorBlock's own
          #  crowd-sourced chapters (competing with YouTube's real ones) and
          #  poi_highlight is a zero-length marker.
          --sponsorblock-mark all,-filler,-chapter,-poi_highlight
        '';
      in
      {
        "yt-dlp.conf".text = youtube-downloader-config-shared;

        "X11/Xresources".text = ''
          Xcursor.theme: Adwaita
        '';
      };
  };
}
