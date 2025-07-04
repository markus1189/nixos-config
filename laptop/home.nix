{ config, pkgs, ... }:

let
  secrets = import ../nixos-shared/secrets.nix;
  mergeAttrList = pkgs.lib.foldl' pkgs.lib.mergeAttrs { };

  nixpkgsMasterSrc = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/master.tar.gz";
  };

  nixpkgsMaster = import nixpkgsMasterSrc { };
in {
  home = {
    stateVersion = "18.09";

    packages = with pkgs; [
      source-code-pro
      dunst
      ndt
      # jrnl fails at tests (2022-02-18)
    ];

    file = {
      "visidatarc" = {
        target = ".visidatarc";
        text = ''
          import json

          options.disp_date_fmt = '%Y-%m-%d %H:%M:%S.%f%z'
          options.disp_float_fmt = '{:.04f}'

          options.reddit_client_id = 'AM6u5feracoVWJ3gJWTnCA'
          options.reddit_client_secret = '${secrets.reddit.visidata}'

          Sheet.bindkey(ALT + '.', 'repeat-input')
          Sheet.bindkey('z' + ALT + '.', 'repeat-last')

          def mh_utc_from_unix(seconds):
            return datetime.datetime.utcfromtimestamp(seconds).strftime('%Y-%m-%dT%H:%M:%SZ')

          def mh_fromjson(s):
            return json.loads(s)
        '';
      };

      "arbtt-categorizer" = {
        target = ".arbtt/categorize.cfg";
        text = ''
          aliases ("Navigator"  -> "firefox",
                   "org_pwmt_zathura" -> "zathura",
                   "telegram-desktop" -> "telegram",
                   "gl" -> "mpv"
                  )

          $idle > 600 ==> tag inactive,

          current window ($title =~ /.*YouTube.*Firefox/ || $title =~ /- mpv$/) ==> tag act:watching,
          current window $program == ["org_pwmt_zathura", "zathura"] ==> tag act:reading,
          current window $program == ["telegram-desktop", "slack"] ==> tag act:chatting,
          current window $program == ["zoom"] ==> tag act:conference,
          current window $title =~ /.*Microsoft Teams.*/ ==> tag act:conference,
          current window $program == "emacs" ==> tag act:coding,
          current window $program == "urxvt" ==> tag act:shell,

          -- Could be useful to see whom I chat with most
          -- current window ($program == "slack" && $title =~ /Slack \|[[:space:]]*([^|]*) \|/) ==> tag slack:$1,

          current window $title =~ /Online Whiteboard for Visual Collaboration/ ==> tag web:miro,
          current window $title =~ /Amazon.de/ ==> tag web:amazon,
          current window $title =~ /DuckDuckGo/ ==> tag web:ddg,
          current window $title =~ /Ultimate AWS Certified.*Udemy/ ==> tag web:aws,

          current window ($program == "emacs" && $title =~ /.*nixos-config.*/) ==> tag emacs:nixos,

          -- tag program:$current.program,
        '';
      };

      "xmonad.hs" = {
        target = ".xmonad/xmonad.hs";
        source = pkgs.myConfigFiles.xmonad;
      };

      "flameshot-config" = {
        target = ".config/flameshot/flameshot.ini";
        text = ''
          [General]
          copyPathAfterSave=true
          disabledTrayIcon=false
          drawColor=#ff0000
          drawThickness=3
          saveAfterCopy=true
          savePath=/home/markus/Screenshots
          uiColor=#ee8903

          [Shortcuts]
          TYPE_PIN=Ctrl+P
        '';
      };

      "edbrowse-config" = {
        target = ".ebrc";
        text = ''
          #  The cookie jar-where we store the http cookies.
          #jar=/home/mylogin/outside/cookies

          webtimer=60
          mailtimer=180

          agent=Lynx/2.8.4rel.1 libwww-FM/2.14
          agent=Mozilla/4.0(compatible;MSIE 7.0;Windows NT 6.1;WOW64;Trident/5.0;SLCC2; .NET CLR 2.0.50727; .NET CLR 3.5.30729; .NET CLR 3.0.30729;Media Center PC 6.0; .NET4.0C; .NET4.0E)

          function:init{
          # Display the size of each file in a directory listing
          ls=s

          # character limit when displaying a line
          ll 700
          # enable readline
          rl+
          }
        '';
      };

      "gtk-bookmarks" = {
        target = ".gtk-bookmarks";
        text = ''
          file:///home/markus/Downloads
          file:///home/markus/Dropbox
          file:///home/markus/repos
          file:///home/markus/Photos/web
          file:///home/markus/Photos/developed
          file:///home/markus/repos/nixos-config
        '';
      };

      "gtkrc2" = {
        target = ".gtkrc-2.0";
        text = ''
          gtk-key-theme-name = "Emacs"
          gtk-cursor-theme-name = "Adwaita"
        '';
      };

      "gtk3" = {
        target = ".config/gtk-3.0/settings.ini";
        text = ''
          [Settings]
          gtk-key-theme-name = Emacs
          gtk-cursor-theme-name = Adwaita
        '';
      };

      "keynavrc" = {
        source = pkgs.callPackage ../nixos-shared/home-manager/keynav { };
        target = ".keynavrc";
      };

      "warpd-config" = {
        text = ''
          hint_activation_key: C-;
          cursor_color: #Ff8c00
          hint_size: 17
          indicator: topright
          indicator_size: 20
          hint_chars: qwertz12345xcvbasdfhjklyuiop67890n/
          buttons: space , question
          oneshot_buttons: Return - /
        '';
        target = ".config/warpd/config";
      };

      "mrconfig" = let
        clonedRepo = owner: repo: ''
          [repos/clones/${repo}]
          checkout = git clone 'https://github.com/${owner}/${repo}' '${repo}'
        '';
        projectRepo = repo: ''
          [repos/projects/${repo}]
          checkout = git clone 'git@github.com:markus1189/${repo}.git'
        '';
      in {
        target = ".mrconfig";
        text = ''
          [repos/nixos-config]
          checkout = git clone 'git@github.com:markus1189/nixos-config.git' 'nixos-config'

          ${clonedRepo "nixos" "nixpkgs"}
          ${clonedRepo "nix-community" "home-manager"}
          ${projectRepo "tiervermittlung-bot"}
          ${projectRepo "hocket"}
        '';
      };

      "jrnl_config" = {
        target = ".config/jrnl/jrnl.yaml";
        text = ''
          colors:
            body: none
            date: none
            tags: none
            title: none
          default_hour: 9
          default_minute: 0
          editor: /nix/store/0dcf13hhk1kl9i0gcq7yjp8p9cp166n4-vim-8.2.2567/bin/vim
          encrypt: false
          highlight: true
          indent_character: '|'
          journals:
            default: /home/markus/Syncthing/jrnl/journal.txt
          linewrap: 79
          tagsymbols: '@'
          template: false
          timeformat: '%Y-%m-%dT%H:%M:%S'
          version: v2.8
        '';
      };

      "mpv_config" = {
        target = ".config/mpv/mpv.conf";
        text = ''
          write-filename-in-watch-later-config=yes
          save-position-on-quit=yes
          osd-msg3="''${time-pos} / ''${duration} [''${playtime-remaining} @ ''${speed}]"
          osd-duration=5000
          osd-font-size=40

          hwdec=no

          volume-max=300

          [stream]
          cache=yes
          demuxer-max-bytes=1000MiB
          demuxer-readahead-secs=60
          cache-secs=600
          prefetch-playlist=yes
        '';
      };

      "mpv_input_conf" = {
        target = ".config/mpv/input.conf";
        text = ''
          ! run sh -c "echo ''${path} >> list.txt && notify-send mpv \"Saved ''${path} to ''${working-directory}/list.txt\""
        '';
      };

      "global-sbt-aliases" = {
        target = ".sbt/1.0/global-aliases.sbt";
        text = ''
          addCommandAlias("r", "reload")
          addCommandAlias("c", "Test/compile")
          addCommandAlias("t", "test")
          addCommandAlias("to", "testOnly")
          addCommandAlias("d", "set javaOptions += \"-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005\"")
          addCommandAlias("do","set javaOptions -= \"-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005\"")
        '';
      };

      "extrakto-conf" = {
        target = ".config/extrakto/extrakto.conf";
        text = ''
          # extrakto filter definitions

          # To override an existing filter just specify the new values.
          # For example, if you prefer to split words on comma as well you can define:
          # [word]
          # regex: ([^][(){}=$─-➿-, \t\n\r]+)

          # define a section per filter
          # each filter must have at least a regex containing one or more capture groups
          # regex:   a python regex expression
          # enabled: is filter active (default True)
          # in_all:  is included in --all (default True)
          # lstrip:  characters to strip from left result
          # rstrip:  characters to strip from right result
          # exclude: exclude result if matching
          # alt2-9:  alternate result (see url)

          [word]
          # "words" consist of anything but the following characters:
          # [](){}=$
          # unicode range 2500-27BF which includes:
          # - Box Drawing
          # - Block Elements
          # - Geometric Shapes
          # - Miscellaneous Symbols
          # - Dingbats
          # unicode range E000-F8FF (private use/Powerline)
          # and whitespace ( \t\n\r)
          # regex: [^][(){}=$\u2500-\u27BF\uE000-\uF8FF \t\n\r]+
          regex: ([^][(){}=$─-➿- \t\n\r]+)
          lstrip: ,:;()[]{}<>'"|
          rstrip: ,:;()[]{}<>'"|.
          in_all: False

          [path]
          # separator: (?=[ \t\n]|"|\(|\[|<|\')?
          # optionally starts with: (~|/)?
          regex: (?:[ \t\n\"([<':]|^)(~|/)?([-~a-zA-Z0-9_+-,.]+/[^ \t\n\r|:"'$%&)>\]]*)
          # exclude transfer speeds like 5k/s or m/s, and page 1/2
          exclude: [kmgKMG]/s$|^\d+/\d+$
          # remove invalid end characters (like punctuation or markdown syntax)
          rstrip: ",):"

          [url]
          regex: (https?://|git@|git://|ssh://|s*ftp://|file:///)([a-zA-Z0-9?=%/_.:,;~@!#$&()*+-]*)
          alt2: ://([^/? ]+)
          # remove invalid end characters (like punctuation or markdown syntax)
          rstrip: ",):"

          [quote]
          regex: ("[^"\n\r]+")
          alt2: "([^"\n\r]+)"

          [s-quote]
          regex: ('[^'\n\r]+')
          alt2: '([^'\n\r]+)'
        '';
      };

      "rofi-config" = {
        target = ".config/rofi/config.rasi";
        text = ''
          @theme "${pkgs.rofi}/share/rofi/themes/Arc-Dark.rasi"
        '';
      };

      "lnav-custom-formats" = {
        target = ".lnav/formats/custom/custom-formats-home-manager.json";
        text = pkgs.lib.strings.toJSON {
          "$schema" = "https://lnav.org/schemas/format-v1.schema.json";
          logback_log = {
            title = "Java Logback Format";
            description = "Log format for most logback formats";
            json = true;
            level-field = "level";
            level = {
              error = "ERROR";
              warning = "WARN";
              info = "INFO";
            };
            timestamp-field = "@timestamp";
            body-field = "message";
          };
        };
      };

      "dungeon crawl stone soup" = {
        target = ".crawlrc";
        text = ''
          travel_delay = -1
          show_travel_trail = true

          default_manual_training = true

          ai := autoinscribe
          ai += of identify:@r1
          ai += curing:@q1
        '';
      };

      "aider" = {
        target = ".aider.conf.yml";
        text = pkgs.lib.strings.toJSON {
          read = [ "CONVENTIONS.md" ];
          auto-commits = false;
          watch-files = true;
          notifications = true;
          gitignore = false; # in global gitignore
          check-update = false; # only via nixpkgs
        };
      };

      "claude-code" = let
        notifier = pkgs.writers.writePython3Bin "claude-code-notifier" {
          flakeIgnore = [ "E501" ];
        } ''
          import json
          import sys
          import subprocess

          input = json.load(sys.stdin)

          title = input.get("title", "<no-title>")
          message = input.get("message", "<no-message>")

          subprocess.run(["${pkgs.dunst}/bin/dunstify", "--urgency=critical", title, message])
        '';
      in {
        target = ".claude/settings.json";
        text = pkgs.lib.strings.toJSON {
          includeCoAuthoredBy = false;
          permissions = {
            allow = [
              "Bash(grep:*)"
              "Bash(rg:*)"
              "Bash(./gradlew test:*)"
              "Bash(notify-send:*)"
              "Bash(git add:*)"
              "Bash(git commit:*)"
            ];
          };
          preferredNotifChannel = "terminal_bell";
          autoUpdaterStatus = "disabled";
          hooks = {
            Notification = [{
              matcher = "";
              hooks = [{
                type = "command";
                command = "${notifier}/bin/claude-code-notifier";
              }];
            }];
          };
        };
      };

      "claude-md" = {
        target = ".claude/CLAUDE.md";
        text = builtins.readFile ../nixos-shared/claude/CLAUDE-global.md;
      };

      "claude-ai-comments" = {
        target = ".claude/commands/ai-comments.md";
        text = builtins.readFile ../nixos-shared/claude/commands/ai-comments.md;
      };

      "claude-ai-bug-hunt" = {
        target = ".claude/commands/bug-hunt.md";
        text = builtins.readFile ../nixos-shared/claude/commands/bug-hunt.md;
      };
    };
  };

  manual = {
    html.enable = true;
    json.enable = true;
    manpages.enable = true;
  };

  programs = let passDir = "$HOME/.local/share/password-store";
  in {
    alacritty = {
      enable = true;
      settings = {
        font.size = 8;
        window = {
          opacity = 0;
          blur = true;
        };
        selection.save_to_clipboard = true;
        mouse.hide_when_typing = true;
        keyboard.bindings = [{
          key = "v";
          mods = "Alt";
          action = "Paste";
        }];
      };
    };

    atuin = {
      enable = true;
      settings = {
        show_preview = true;
        max_preview_height = 5;
        history_filter = [ "DONOTTRACK" ];
        inline_height = 25;
        update_check = false;
      };
    };

    bash.enable = true;

    zsh = {
      enable = true;
      history = rec {
        expireDuplicatesFirst = true;
        extended = true;
        ignoreDups = true;
        ignorePatterns = [ "rm *" ];
        ignoreSpace = true;
        save = 999999999;
        size = save;
        share = true;
      };

      shellAliases = {
        "aws-vault" =
          "aws-vault --backend=pass --pass-dir=${passDir} --pass-cmd=pass --pass-prefix=aws";
      };

      initExtra = ''
        source ${pkgs.ndtSources.zsh-histdb}/sqlite-history.zsh
        autoload -Uz add-zsh-hook
      '';
    };

    direnv = {
      enable = true;
      enableBashIntegration = true;
      nix-direnv = {
        enable = true;
        package = nixpkgsMaster.nix-direnv;
      };
      enableZshIntegration = true;
    };

    firefox = (pkgs.callPackage ../nixos-shared/home-manager/firefox/default.nix
      { }).value;

    git =
      (pkgs.callPackage ../nixos-shared/home-manager/git/default.nix { }).value;

    jujutsu = {
      enable = true;
      settings = {
        user = {
          name = "Markus Hauck";
          email = "markus1189@gmail.com";
        };

        merge-tools = {
          ediff = {
            merge-args = [ "merge" "$left" "$right" "$base" "$output" ];
            diff-args = [ "diff-dir" "$left" "$right" ];
            edit-args = [ "diff-dir" "$left" "$right" ];
            program =
              "${pkgs.myScripts.emacs-ediff-dispatch}/bin/ediff-dispatch";
          };
        };

        revsets = { log = "@ | ancestors(immutable_heads().., 5) | trunk()"; };

        ui = { default-command = [ "log" ]; };
      };
    };

    vim =
      (pkgs.callPackage ../nixos-shared/home-manager/vim/default.nix { }).value;

    mpv = {
      enable = true;
      package = pkgs.mpv.override {
        scripts = with pkgs.mpvScripts; [ sponsorblock mpris smartskip ];
      };
    };

    password-store = {
      enable = true;
      package = pkgs.pass.withExtensions (exts: [ exts.pass-otp ]);
      settings = { PASSWORD_STORE_DIR = passDir; };
    };

    autorandr = {
      enable = true;
      profiles = let
        internalDisplay =
          "00ffffffffffff0009e5ec0800000000011d0104b523137802df50a35435b5260f50540000000101010101010101010101010101010152d000a0f0703e803020350058c21000001a00000000000000000000000000000000001a000000fe00424f452048460a202020202020000000fe004e4531353651554d2d4e36410a017702030f00e3058000e6060501737321000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008b";
        firstHalf =
          "00ffffffffffff0010acf1a04c5234300a1e010380582578eeee95a3544c99260f5054a54b00714f81008180a940d1c00101010101012d5080a070402e6030203a00706f3100001a000000ff00354b4330333033353034524c0a000000fc0044454c4c20553338313844570a000000fd001855197322000a20202020202001f8020322f14d9005040302071601141f12135a230907078301000067030c0020003844023a801871382d40582c4500706f3100001e565e00a0a0a0295030203500706f3100001acd4600a0a0381f4030203a00706f3100001a134c00a0f040176030203a00706f3100001a000000000000000000000000000000000000000000c8";
        secondHalf =
          "00ffffffffffff0010acf4a04c5234300a1e0104b55825783eee95a3544c99260f5054a54b00714f81008180a940d1c00101010101012d5080a070402e6030203a00706f3100001a000000ff00354b4330333033353034524c0a000000fc0044454c4c20553338313844570a000000fd001855197328000a202020202020016902031af14d9005040302071601141f12135a2309070783010000023a801871382d40582c4500706f3100001e565e00a0a0a0295030203500706f3100001acd4600a0a0381f4030203a00706f3100001a4c9a00a0f0402e6030203a00706f3100001a134c00a0f040176030203a00706f3100001a0000000000000000000000ea";
      in {
        "mobile" = {
          fingerprint = {
            "eDP-1" =
              "00ffffffffffff0009e5ec0800000000011d0104b523137802df50a35435b5260f50540000000101010101010101010101010101010152d000a0f0703e803020350058c21000001a00000000000000000000000000000000001a000000fe00424f452048460a202020202020000000fe004e4531353651554d2d4e36410a017702030f00e3058000e6060501737321000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008b";
          };
          config = {
            "DP-1".enable = false;
            "DP-2".enable = false;
            "DP-3".enable = false;
            "eDP-1" = {
              enable = true;
              crtc = 0;
              primary = true;
              position = "0x0";
              mode = " 1920x1080";
              rate = "60.00";
            };
          };
        };

        "double" = {
          fingerprint = {
            "DP-1" = firstHalf;
            "DP-2" = secondHalf;
            "eDP-1" = internalDisplay;
          };

          config = {
            "DP-1" = {
              enable = true;
              position = "1920x0";
              mode = "1920x1600";
              rate = "60.00";
            };
            "DP-2" = {
              primary = true;
              enable = true;
              position = "0x0";
              mode = "1920x1600";
              rate = "60.00";
            };
            "DP-3".enable = false;
            "eDP-1".enable = false;
          };
        };

        "double2" = {
          fingerprint = {
            "DP-1" = firstHalf;
            "DP-3" = secondHalf;
            "eDP-1" = internalDisplay;
          };

          config = {
            "DP-1" = {
              enable = true;
              position = "1920x0";
              mode = "1920x1600";
              rate = "60.00";
            };
            "DP-2".enable = false;
            "DP-3" = {
              primary = true;
              enable = true;
              position = "0x0";
              mode = "1920x1600";
              rate = "60.00";
            };
            "eDP-1".enable = false;
          };
        };

        "double3" = {
          fingerprint = {
            "DP-1" = "*";
            "DP-2" = firstHalf;
            "DP-3" = secondHalf;
            "eDP-1" = internalDisplay;
          };

          config = {
            "DP-1".enable = false;
            "DP-2" = {
              enable = true;
              position = "1920x0";
              mode = "1920x1600";
              rate = "60.00";
            };
            "DP-3" = {
              primary = true;
              enable = true;
              position = "0x0";
              mode = "1920x1600";
              rate = "60.00";
            };
            "eDP-1".enable = false;
          };
        };
      };
    };

    sioyek = {
      enable = true;
      bindings = {
        "next_page" = "J";
        "previous_page" = "K";
      };
    };
  };

  services = {
    flameshot.enable = true;

    dunst = (pkgs.callPackage ../nixos-shared/home-manager/dunst/default.nix
      { }).value;

    gpg-agent = {
      enable = true;
      defaultCacheTtl = 60 * 60 * 9;
      maxCacheTtl = 60 * 60 * 12;
    };

  };

  fonts = { fontconfig = { enable = true; }; };

  systemd.user.services = {
    arbtt = let arbttPackage = pkgs.haskellPackages.arbtt;
    in {
      Unit = { Description = "arbtt statistics capture service"; };

      Install = { WantedBy = [ "graphical-session.target" ]; };

      Service = {
        Type = "simple";
        ExecStart =
          "${arbttPackage}/bin/arbtt-capture --logfile=%h/.arbtt/capture.log --sample-rate=${
            toString 60
          }";
        Restart = "always";
      };
    };
  };

  xdg = {
    mimeApps = let
      zathuraDesktop =
        "${pkgs.zathura}/share/applications/org.pwmt.zathura.desktop";
    in {
      enable = true;
      associations.added = {
        "application/pdf" = [ "org.pwmt.zathura.desktop" ];
        "x-scheme-handler/tg" = [ "telegramdesktop.desktop" ];
        "x-scheme-handler/msteams" = [ "teams.desktop" ];
      };
      defaultApplications = {
        "application/pdf" = [ "org.pwmt.zathura.desktop" ];
        "x-scheme-handler/tg" = [ "telegramdesktop.desktop" ];
        "x-scheme-handler/msteams" = [ "teams.desktop" ];
      };
    };
  };
}
