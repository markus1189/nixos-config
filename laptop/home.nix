{
  config,
  pkgs,
  osConfig,
  inputs,
  ...
}:

let
  otelCollector = pkgs.callPackage (import ../nixos-shared/home-manager/otel-collector {
    dataDir = "${config.home.homeDirectory}/.local/share/claude-otel";
  }) { };

in
{
  imports = [
    ./atuin-sync.nix
    # Sets programs.git and installs the global gitleaks config (xdg.configFile).
    ../nixos-shared/home-manager/git/default.nix
    ../nixos-shared/home-manager/dunst/default.nix
    ../nixos-shared/home-manager/firefox/default.nix
    ../nixos-shared/home-manager/rumdl/default.nix
    ../nixos-shared/home-manager/starship/default.nix
    ../nixos-shared/home-manager/vim/default.nix
    ../nixos-shared/home-manager/zsh/default.nix
    ../nixos-shared/home-manager/xmonad-autostart/default.nix
  ];

  home = {
    stateVersion = "18.09";

    packages = with pkgs; [
      source-code-pro
      dunst
      myScripts.mpv-watch-later-overview
      myScripts.claude-history
      myScripts.addToRaindropScript
    ];

    file =
      let
        claudeConfig = pkgs.callPackage ../nixos-shared/home-manager/claude-code {
          enableSoundHooks = true;
          enableDenyRules = true;
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
              requesty = {
                name = "Requesty Codecentric";
                npm = "@ai-sdk/openai-compatible";
                options = {
                  baseURL = "https://router.eu.requesty.ai/v1";
                  apiKey = "{env:REQUESTY_API_KEY_CC}";
                };

                models = {
                  "vertex/gemini-3.5-flash@eu" = {
                    name = "Gemini 3.5 Flash";
                  };

                  "azure/gpt-5.4@swedencentral" = {
                    name = "GPT 5.4 Chat";
                  };

                  "vertex/claude-opus-5@eu" = {
                    name = "Claude Opus 5";
                    # No thinking block: like Sonnet 5, Opus 5 rejects the legacy
                    # thinking.type="enabled"/budgetTokens API (HTTP 400) and requires
                    # thinking.type="adaptive"+output_config.effort. opencode's model
                    # config schema only accepts type enum ["enabled","disabled"], so
                    # adaptive can't be expressed here for a custom-provider model.
                    # Opus 5 thinks adaptively by default when no thinking is sent.
                    modalities = {
                      input = [
                        "text"
                        "image"
                      ];
                      output = [ "text" ];
                    };
                  };

                  "vertex/claude-sonnet-5@eu" = {
                    name = "Claude Sonnet 5";
                    # No thinking block: Sonnet 5 dropped the legacy
                    # thinking.type="enabled" API (returns HTTP 400) and requires
                    # thinking.type="adaptive"+output_config.effort. opencode's model
                    # config schema only accepts type enum ["enabled","disabled"], so
                    # adaptive can't be expressed here for a custom-provider model.
                    # Run non-thinking until opencode exposes adaptive as a config type.
                    modalities = {
                      input = [
                        "text"
                        "image"
                      ];
                      output = [ "text" ];
                    };
                  };

                  "vertex/claude-haiku-4-5@europe-west1" = {
                    name = "Claude Haiku 4.5";
                    thinking = {
                      type = "enabled";
                      budgetTokens = 16000;
                    };
                    modalities = {
                      input = [
                        "text"
                        "image"
                      ];
                      output = [ "text" ];
                    };
                  };

                  "nebius/moonshotai/kimi-k2.5" = {
                    name = "Kimi K2.5 (OpenWeight EU)";
                  };
                };
              };
            };
          };
        };

        "opencode-tui-config" = {
          target = ".config/opencode/tui.json";
          text = pkgs.lib.strings.toJSON {
            "$schema" = "https://opencode.ai/tui.json";
            attention = {
              enabled = true;
              notifications = true;
              sound = false;
            };
          };
        };

        "opencode-plugin-terminal-bell" = {
          target = ".config/opencode/plugin/terminal-bell.ts";
          text = builtins.readFile ../nixos-shared/home-manager/opencode/terminal-bell.ts;
        };

        "opencode-plugin-sounds" = {
          target = ".config/opencode/plugin/sounds.ts";
          source = pkgs.replaceVars ../nixos-shared/home-manager/opencode/sounds.ts {
            aplay = pkgs.alsa-utils;
            involvedNotificationSound = "${../nixos-shared/claude/sounds/involved-notification.wav}";
            pullOutSound = "${../nixos-shared/claude/sounds/pull-out-551.wav}";
            forSureSound = "${../nixos-shared/claude/sounds/for-sure-576.wav}";
            happyToHelpSound = "${../nixos-shared/claude/sounds/happy-to-help-notification-sound.wav}";
            comeHereSound = "${../nixos-shared/claude/sounds/come-here-notification.wav}";
            intuitionSound = "${../nixos-shared/claude/sounds/intuition-561.wav}";
            timeIsNowSound = "${../nixos-shared/claude/sounds/time-is-now-585.wav}";
            justMaybeSound = "${../nixos-shared/claude/sounds/just-maybe-577.wav}";
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
            options.reddit_client_secret = open('/run/agenix/reddit-visidata').read().strip()

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
            ; flameshot 14 asks which monitor to capture; take the one the
            ; cursor is on instead. X11 only -- ignored on Wayland.
            captureActiveMonitor=true
            copyPathAfterSave=true
            disabledTrayIcon=false
            drawColor=#ff0000
            drawThickness=3
            saveAfterCopy=true
            ; flameshot 14 defaults to xdg-desktop-portal capture; xmonad has no portal
            useX11LegacyScreenshot=true
            ; Symlink managed by cdt/Emacs to today's Stuff directory
            savePath=${config.home.homeDirectory}/Stuff/Today
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
            file://${config.home.homeDirectory}/Downloads
            file://${config.home.homeDirectory}/Dropbox
            file://${config.home.homeDirectory}/repos
            file://${config.home.homeDirectory}/Photos/web
            file://${config.home.homeDirectory}/Photos/developed
            file://${config.home.homeDirectory}/repos/nixos-config
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
              ${projectRepo "bookbuddy.koplugin"}
              ${projectRepo "ciqt"}
              ${projectRepo "tools"}
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
            # Pin render to the Intel iGPU — libplacebo otherwise defaults to the
            # discrete NVIDIA dGPU, which wakes it and forces an Intel->NVIDIA copy.
            # (p1 is Intel-only, so this name simply won't match there and is ignored.)
            vulkan-device="Intel(R) Graphics (ARL)"
            hwdec=vaapi,nvdec-copy

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

            * {
              accent:                       #f3843d;
              accent-text:                  #000000;
              selected-normal-background:   @accent;
              selected-normal-foreground:   @accent-text;
              selected-active-background:   @accent;
              selected-active-foreground:   @accent-text;
              selected-urgent-background:   #cc4444;
              selected-urgent-foreground:   #ffffff;
            }

            element selected,
            element selected.active,
            element-text selected,
            element-text selected.active {
              background-color: @accent;
              text-color:       @accent-text;
            }
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
      // claudeConfig.agentFiles
      // opencodeConfig.markdownFiles
      // piAgentConfig.linkedFiles;
  };

  programs =
    let
      passDir = "$HOME/.local/share/password-store";
    in
    {
      zathura = {
        enable = true;
        options = {
          selection-clipboard = "clipboard";
        };
      };

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
              opacity = 0.92;
              blur = false;
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

          app-notifications = "no-clipboard-copy";

          background-opacity = 0.95;
          background = "#0d0d0d";
          background-blur = false;
          gtk-titlebar = false;

          shell-integration-features = "no-cursor";

          confirm-close-surface = false;

          font-feature = "-calt, -liga, -dlig";

          bell-features = "no-title";

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

          # Self-hosted sync via SSH tunnel to nuc. Tunnel + sync is driven
          # by the atuin-sync systemd user timer (see atuin-sync.nix).
          sync_address = "http://127.0.0.1:49888";
          auto_sync = false;
        };
      };

      bash.enable = true;

      direnv = {
        enable = true;
        enableBashIntegration = true;
        nix-direnv = {
          enable = true;
          package = pkgs.masterPkgs.nix-direnv;
        };
        enableZshIntegration = true;
      };

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
        # Profiles are per-host (different panels + connector enumeration):
        # p1/home.nix and p1g8/home.nix, merged in via
        # home-manager.users.*.imports in each configuration.nix.
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
    picom = {
      enable = true;
      backend = "glx";
      vSync = true;
    };

    flameshot.enable = true;

    clipcat = {
      enable = true;
      # We have our own zsh clipboard widget (nixos-shared/zsh.nix) and the
      # integration would bind ^\ / ^] - leave it off to avoid a turf war.
      enableZshIntegration = false;

      daemonSettings = {
        daemonize = true; # overridden by the unit's --no-daemon; kept for module parity
        max_history = 9999; # was CM_MAX_CLIPS=9999
        # MUST stay false (and be set explicitly - clipcat defaults it true):
        # with sync on, clipcatd takes ownership of both PRIMARY and CLIPBOARD,
        # which breaks its own change-detection (BadAtom on GetProperty) and it
        # captures nothing. False = watch both independently, like clipmenu did.
        synchronize_selection_with_clipboard = false;

        watcher = {
          enable_clipboard = true;
          enable_primary = true;
          capture_image = true;
          # Skip clips tagged sensitive by password managers.
          sensitive_mime_types = [ "x-kde-passwordManagerHint" ];
          # Never store clips matching these regexes (tune to taste). Example:
          #   "^[0-9]{6,8}$"  -> bare OTP codes
          denied_text_regex_patterns = [ ];
        };

        # dunst already handles our toasts; clipcat notifying on every copy is noise.
        desktop_notification.enable = false;
      };

      menuSettings = {
        # Use the loopback TCP endpoint, not the default unix socket: the
        # socket can lag on first boot (daemon comes up TCP-only briefly),
        # which makes the keybinding silently fail. TCP is always up.
        server_endpoint = "http://127.0.0.1:45045";
        finder = "rofi";
        rofi = {
          menu_length = 30; # was CM_HISTLENGTH=30
          line_length = 100;
          menu_prompt = "Clipcat";
        };
      };

      # Keep clipcatctl on the same endpoint so CLI debugging Just Works.
      ctlSettings = {
        server_endpoint = "http://127.0.0.1:45045";
      };
    };

    gpg-agent = {
      enable = true;
      defaultCacheTtl = 60 * 60 * 9;
      maxCacheTtl = 60 * 60 * 12;
    };

    pass-secret-service = {
      enable = true;
      # mkForce needed: password-store module auto-sets storePath to
      # PASSWORD_STORE_DIR ("$HOME/..."), but $HOME isn't expanded in
      # systemd ExecStart args, causing pass_secret_service to crash.
      storePath = pkgs.lib.mkForce "${config.home.homeDirectory}/.local/share/password-store";
    };

  };

  # The boot-time desktop, one supervised systemd user unit per program; see
  # ../nixos-shared/home-manager/xmonad-autostart for why they hang off
  # xmonad-session.target rather than graphical-session.target.
  #
  # Which workspace each window lands on is xmonad's business, not systemd's:
  # myManageHook shifts them by WM_CLASS. The two windows that had no
  # distinguishing class of their own get an explicit instance-name marker
  # here, matched by the ws1Resources / ws4Resources rules in xmonad.hs.
  my.xmonadAutostart = {
    # WS1: the default tmux session -- tmx attaches to the one the xsession
    # wrapper pre-creates, or creates it if that failed.
    term-default.command =
      "${config.programs.ghostty.package}/bin/ghostty --x11-instance-name=ws1-default"
      + " -e ${pkgs.myScripts.tmx}/bin/tmx default";

    # WS2
    firefox.command = "${config.programs.firefox.finalPackage}/bin/firefox";

    # WS4. Deliberately a plain GUI emacs rather than an emacsclient against a
    # services.emacs daemon: emacs-config.el applies a good deal of its
    # appearance to the frame that exists at load time, of which a daemon has
    # none, so daemon-created frames come up looking nothing like this one.
    #
    # -name sets the WM_CLASS instance *and* freezes the title;
    # emacs-config.el clears the name again once the frame is up, which leaves
    # the marker in place and gives the buffer name back to the title bar.
    emacs-main.command = "${pkgs.emacs}/bin/emacs --name emacs-main";

    # WS8
    telegram.command = "${pkgs.telegram-desktop}/bin/Telegram";
    signal.command = "${pkgs.signal-desktop}/bin/signal-desktop";
    slack.command = "${pkgs.slack}/bin/slack";
    spotify.command = "${pkgs.spotify}/bin/spotify";
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

    otel-collector = otelCollector.service;
  };

  xdg = {
    mimeApps = {
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
