{ config, pkgs, ... }:

let
  secrets = import ../nixos-shared/secrets.nix;
  mergeAttrList = pkgs.lib.foldl' pkgs.lib.mergeAttrs { };

  nixpkgsMasterSrc = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/master.tar.gz";
  };

  nixpkgsMaster = import nixpkgsMasterSrc { };
in
{
  home = {
    stateVersion = "18.09";

    packages = with pkgs; [
      source-code-pro
      dunst
      ndt
      # jrnl fails at tests (2022-02-18)
      myScripts.mpv-watch-later-overview
      myScripts.claude-history
    ];

    file =
      let
        claudeConfig = pkgs.callPackage ../nixos-shared/home-manager/claude-code {
          enableSoundHooks = true;
          enableDenyRules = true;
          enablePythonPathCheck = true;
          additionalAllowedCommands = [
            "Bash(git commit:*)"
            "Bash(git show:*)"
          ];
        };
        opencodeConfig = pkgs.callPackage ../nixos-shared/home-manager/opencode { };
        piAgentConfig = pkgs.callPackage ../nixos-shared/home-manager/pi-agent {
          globalMdText = claudeConfig.globalClaudeMd.text;
        };
      in
      {
        "claude-code" = claudeConfig.settings;
        "claude-md" = claudeConfig.globalClaudeMd;

        "gemini-global" = {
          target = ".gemini/GEMINI.md";
          inherit (claudeConfig.globalClaudeMd) text;
        };

        "copilot-instructions-global" = {
          target = ".copilot/copilot-instructions.md";
          inherit (claudeConfig.globalClaudeMd) text;
        };

        "opencode-config" = {
          target = ".config/opencode/opencode.json";
          text = pkgs.lib.strings.toJSON {
            "$schema" = "https://opencode.ai/config.json";
            tui = {
              scroll_speed = 5;
            };

            permission = {
              bash = {
                "git commit" = "ask";
                "rm -f" = "ask";
                "rm -rf" = "deny";
              };
            };

            provider = {
              portkey = {
                name = "Portkey Codecentric";
                npm = "@ai-sdk/openai-compatible";
                options = {
                  baseURL = "https://api.portkey.ai/v1";
                  apiKey = "{env:PORTKEY_API_KEY_CC}";
                };

                models = {
                  "@gcp-gemini/gemini-3-pro-preview" = {
                    name = "Gemini 3 Pro Preview";
                  };

                  "@azure-openai-foundry/gpt-5.1-chat" = {
                    name = "GPT 5.1 Chat";
                  };

                  "@bedrock/eu.anthropic.claude-opus-4-5-20251101-v1:0" = {
                    name = "Claude Opus 4.5";
                    thinking = {
                      type = "enabled";
                      budgetTokens = 16000;
                    };
                    modalities = {
                      input = ["text" "image"];
                      output = ["text"];
                    };
                  };

                  "@bedrock/eu.anthropic.claude-sonnet-4-5-20250929-v1:0" = {
                    name = "Claude Sonnet 4.5";
                    thinking = {
                      type = "enabled";
                      budgetTokens = 16000;
                    };
                    modalities = {
                      input = ["text" "image"];
                      output = ["text"];
                    };
                  };

                  "@bedrock/eu.anthropic.claude-haiku-4-5-20251001-v1:0" = {
                    name = "Claude Haiku 4.5";
                    thinking = {
                      type = "enabled";
                      budgetTokens = 16000;
                    };
                    modalities = {
                      input = ["text" "image"];
                      output = ["text"];
                    };
                  };

                  "@ovh/Qwen3-Coder-30B-A3B-Instruct" = {
                    name = "Qwen3 Coder 30B A3B Instruct";
                  };
                };
              };
            };
          };
        };

        "opencode-plugin-terminal-bell" = {
          target = ".config/opencode/plugin/terminal-bell.ts";
          text = builtins.readFile ../nixos-shared/home-manager/opencode/terminal-bell.ts;
        };

        "opencode-plugin-sounds" = {
          target = ".config/opencode/plugin/sounds.ts";
          source = pkgs.mutate ../nixos-shared/home-manager/opencode/sounds.ts {
            inherit (pkgs) alsa-utils;
            aplay = pkgs.alsa-utils;
            involvedNotificationSound = ../nixos-shared/claude/sounds/involved-notification.wav;
            pullOutSound = ../nixos-shared/claude/sounds/pull-out-551.wav;
            forSureSound = ../nixos-shared/claude/sounds/for-sure-576.wav;
            happyToHelpSound = ../nixos-shared/claude/sounds/happy-to-help-notification-sound.wav;
            comeHereSound = ../nixos-shared/claude/sounds/come-here-notification.wav;
            intuitionSound = ../nixos-shared/claude/sounds/intuition-561.wav;
            timeIsNowSound = ../nixos-shared/claude/sounds/time-is-now-585.wav;
            justMaybeSound = ../nixos-shared/claude/sounds/just-maybe-577.wav;
          };
        };

        "opencode-global-rules" = {
          target = ".config/opencode/AGENTS.md";
          inherit (claudeConfig.globalClaudeMd) text;
        };

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

        "mrconfig" =
          let
            clonedRepo = owner: repo: ''
              [repos/clones/${repo}]
              checkout = git clone 'https://github.com/${owner}/${repo}' '${repo}'
            '';
            projectRepo = repo: ''
              [repos/projects/${repo}]
              checkout = git clone 'git@github.com:markus1189/${repo}.git'
            '';
          in
          {
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

            # Video output
            vo=gpu-next
            gpu-api=vulkan
            hwdec=auto-safe

            # Debanding
            deband=yes
            deband-iterations=2
            deband-threshold=35
            deband-range=16
            deband-grain=5

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

        "emacs-dired-desktop" = {
          target = ".local/share/applications/emacs-dired.desktop";
          text = ''
            [Desktop Entry]
            Name=Emacs (Dired)
            Exec=emacsclient -c %u
            Type=Application
            MimeType=inode/directory;
            NoDisplay=true
          '';
        };

      }
      // claudeConfig.markdownFiles
      // opencodeConfig.markdownFiles
      // piAgentConfig.linkedFiles;
  };

  manual = {
    html.enable = true;
    json.enable = true;
    manpages.enable = true;
  };

  programs =
    let
      passDir = "$HOME/.local/share/password-store";
    in
    {
      starship = (pkgs.callPackage ../nixos-shared/home-manager/starship/default.nix { }).value;


      alacritty = # config documentation at https://alacritty.org/config-alacritty.html
        let
          emacsclient-jump = pkgs.writeShellScript "emacsclient-jump" ''
            # Parse argument: /path/to/file or /path/to/file:123
            input="$1"

            if [[ "$input" =~ ^(.+):([0-9]+)$ ]]; then
              # Contains line number
              filepath="''${BASH_REMATCH[1]}"
              linenum="''${BASH_REMATCH[2]}"
              exec ${pkgs.emacs}/bin/emacsclient -n "+''${linenum}" "''${filepath}"
            else
              # No line number
              exec ${pkgs.emacs}/bin/emacsclient -n "''${input}"
            fi
          '';
        in
        {
          enable = true;
          settings = {
          font.size = 9;
          window = {
            opacity = 0;
            blur = true;
          };
          selection.save_to_clipboard = true;
          mouse.hide_when_typing = true;
          keyboard.bindings = [
            {
              key = "v";
              mods = "Alt";
              action = "Paste";
            }
            {
              key = "F";
              mods = "Control|Shift";
              action = "None";
            }
          ];
          hints = {
            enabled = [
              # Default: URL/hyperlink hints (preserving Alacritty default)
              {
                command = "xdg-open";
                hyperlinks = true;
                post_processing = true;
                persist = false;
                mouse = {
                  enabled = false;
                };
                binding = {
                  key = "O";
                  mods = "Control|Shift";
                };
                regex = "(ipfs:|ipns:|magnet:|mailto:|gemini://|gopher://|https://|http://|news:|file:|git://|ssh:|ftp://)[^\\u0000-\\u001f\\u007f-\\u009f<>\"\\\\s{-}\\\\^⟨⟩`]+";
              }
              # File path hints - open with emacsclient (FF style, supports :line notation)
              {
                regex = "(~?/(?:\\\\\\\\.|\\\\S)+)(:[0-9]+)?";
                command = "${emacsclient-jump}";
                post_processing = true;
                mouse = {
                  enabled = false;
                  mods = "None";
                };
                binding = {
                  key = "F";
                  mods = "Control|Shift";
                };
              }
              # Git commit hash hints - copy to clipboard
              {
                regex = "[0-9a-f]{7,40}";
                action = "Select";
                mouse = {
                  enabled = false;
                  mods = "None";
                };
                binding = {
                  key = "C";
                  mods = "Control|Shift";
                };
              }
            ];
          };
        };
      };

      ghostty = {
        enable = true;
        enableZshIntegration = true;
        settings = {
          mouse-hide-while-typing = true;

          copy-on-select = "clipboard";

          background-opacity = 0;
          background-blur = true;
          gtk-titlebar = false;

          shell-integration-features = "no-cursor";

          confirm-close-surface = false;

          font-feature = "-calt, -liga, -dlig";

          keybind = [
            "alt+v=paste_from_clipboard"
            "ctrl+shift+f=unbind"
            "ctrl+shift+o=unbind"
            "ctrl+shift+t=unbind"
            "ctrl+shift+p=unbind"
            "ctrl+shift+i=unbind"
          ];
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

      zsh =
        (pkgs.callPackage ../nixos-shared/home-manager/zsh/default.nix { inherit pkgs passDir; }).value;

      direnv = {
        enable = true;
        enableBashIntegration = true;
        nix-direnv = {
          enable = true;
          package = nixpkgsMaster.nix-direnv;
        };
        enableZshIntegration = true;
      };

      firefox = (pkgs.callPackage ../nixos-shared/home-manager/firefox/default.nix { }).value;

      git = (pkgs.callPackage ../nixos-shared/home-manager/git/default.nix { }).value;

      delta = {
        enable = true;
        enableGitIntegration = true;
        options = {
          navigate = true;
          features = "side-by-side line-numbers decorations";
        };
      };

      jujutsu = {
        enable = true;
        settings = {
          user = {
            name = "Markus Hauck";
            email = "markus1189@gmail.com";
          };

          merge-tools = {
            ediff = {
              merge-args = [
                "merge"
                "$left"
                "$right"
                "$base"
                "$output"
              ];
              diff-args = [
                "diff-dir"
                "$left"
                "$right"
              ];
              edit-args = [
                "diff-dir"
                "$left"
                "$right"
              ];
              program = "${pkgs.myScripts.emacs-ediff-dispatch}/bin/ediff-dispatch";
            };
          };

          revsets = {
            log = "@ | ancestors(immutable_heads().., 5) | trunk()";
          };

          ui = {
            default-command = [ "log" ];
          };
        };
      };

      vim = (pkgs.callPackage ../nixos-shared/home-manager/vim/default.nix { }).value;

      mpv = {
        enable = true;
        package = pkgs.mpv.override {
          scripts = with pkgs.mpvScripts; [
            sponsorblock
            mpris
            smartskip
          ];
        };
      };

      password-store = {
        enable = true;
        package = pkgs.pass.withExtensions (exts: [ exts.pass-otp ]);
        settings = {
          PASSWORD_STORE_DIR = passDir;
        };
      };

      autorandr = {
        enable = true;
        profiles =
          let
            internalDisplay = "00ffffffffffff0009e5ec0800000000011d0104b523137802df50a35435b5260f50540000000101010101010101010101010101010152d000a0f0703e803020350058c21000001a00000000000000000000000000000000001a000000fe00424f452048460a202020202020000000fe004e4531353651554d2d4e36410a017702030f00e3058000e6060501737321000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008b";
            firstHalf = "00ffffffffffff0010acf1a04c5234300a1e010380582578eeee95a3544c99260f5054a54b00714f81008180a940d1c00101010101012d5080a070402e6030203a00706f3100001a000000ff00354b4330333033353034524c0a000000fc0044454c4c20553338313844570a000000fd001855197322000a20202020202001f8020322f14d9005040302071601141f12135a230907078301000067030c0020003844023a801871382d40582c4500706f3100001e565e00a0a0a0295030203500706f3100001acd4600a0a0381f4030203a00706f3100001a134c00a0f040176030203a00706f3100001a000000000000000000000000000000000000000000c8";
            secondHalf = "00ffffffffffff0010acf4a04c5234300a1e0104b55825783eee95a3544c99260f5054a54b00714f81008180a940d1c00101010101012d5080a070402e6030203a00706f3100001a000000ff00354b4330333033353034524c0a000000fc0044454c4c20553338313844570a000000fd001855197328000a202020202020016902031af14d9005040302071601141f12135a2309070783010000023a801871382d40582c4500706f3100001e565e00a0a0a0295030203500706f3100001acd4600a0a0381f4030203a00706f3100001a4c9a00a0f0402e6030203a00706f3100001a134c00a0f040176030203a00706f3100001a0000000000000000000000ea";
          in
          {
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

            "homeoffice" = {
              fingerprint = {
                "DP-2" =
                  "00ffffffffffff0010acefa04c5234300a1e010380582578eeee95a3544c99260f5054a54b00714f81008180a940d1c00101010101012d5080a070402e6030203a00706f3100001a000000ff00354b4330333033353034524c0a000000fc0044454c4c20553338313844570a000000fd001855197322000a20202020202001fa020322f14d9005040302071601141f12135a230907078301000067030c0010003844023a801871382d40582c4500706f3100001e565e00a0a0a0295030203500706f3100001acd4600a0a0381f4030203a00706f3100001a134c00a0f040176030203a00706f3100001a000000000000000000000000000000000000000000d8";
                "DP-3" =
                  "00ffffffffffff0010acf4a04c5234300a1e0104b55825783eee95a3544c99260f5054a54b00714f81008180a940d1c00101010101012d5080a070402e6030203a00706f3100001a000000ff00354b4330333033353034524c0a000000fc0044454c4c20553338313844570a000000fd001855197328000a202020202020016902031af14d9005040302071601141f12135a2309070783010000023a801871382d40582c4500706f3100001e565e00a0a0a0295030203500706f3100001acd4600a0a0381f4030203a00706f3100001a4c9a00a0f0402e6030203a00706f3100001a134c00a0f040176030203a00706f3100001a0000000000000000000000000000000000000000ea";
                "eDP-1" = internalDisplay;
              };

              config = {
                "DP-1".enable = false;
                "DP-2" = {
                  enable = true;
                  crtc = 2;
                  position = "1920x0";
                  mode = "1920x1600";
                  rate = "59.95";
                };
                "DP-3" = {
                  primary = true;
                  enable = true;
                  crtc = 0;
                  position = "0x0";
                  mode = "1920x1600";
                  rate = "59.95";
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

      fd = {
        enable = true;
      };
    };

  services = {
    flameshot.enable = true;

    dunst = (pkgs.callPackage ../nixos-shared/home-manager/dunst/default.nix { }).value;

    gpg-agent = {
      enable = true;
      defaultCacheTtl = 60 * 60 * 9;
      maxCacheTtl = 60 * 60 * 12;
    };

  };

  fonts = {
    fontconfig = {
      enable = true;
    };
  };

  systemd.user.services = {
    arbtt =
      let
        arbttPackage = pkgs.haskellPackages.arbtt;
      in
      {
        Unit = {
          Description = "arbtt statistics capture service";
        };

        Install = {
          WantedBy = [ "graphical-session.target" ];
        };

        Service = {
          Type = "simple";
          ExecStart = "${arbttPackage}/bin/arbtt-capture --logfile=%h/.arbtt/capture.log --sample-rate=${toString 60}";
          Restart = "always";
        };
      };
  };

  xdg = {
    mimeApps =
      let
        zathuraDesktop = "${pkgs.zathura}/share/applications/org.pwmt.zathura.desktop";
      in
      {
        enable = true;
        associations.added = {
          "application/pdf" = [ "org.pwmt.zathura.desktop" ];
          "x-scheme-handler/tg" = [ "telegramdesktop.desktop" ];
          "x-scheme-handler/msteams" = [ "teams.desktop" ];
        };
        defaultApplications = {
          "application/pdf" = [ "org.pwmt.zathura.desktop" ];
          "inode/directory" = [ "emacs-dired.desktop" ];
          "x-scheme-handler/tg" = [ "telegramdesktop.desktop" ];
          "x-scheme-handler/msteams" = [ "teams.desktop" ];
        };
      };
  };
}
