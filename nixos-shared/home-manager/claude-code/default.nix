{ pkgs
, enableSoundHooks ? false
, enableDenyRules ? false
, enablePythonPathCheck ? false
, enableDangerousCommandCheck ? true
, additionalAllowedCommands ? []
, ...
}:

let
  # Helper function to automatically discover and configure markdown files
  autoConfigMarkdownFiles =
    sourceDir: targetSubdir: namePrefix:
    let
      files = builtins.readDir sourceDir;
      isMarkdownFile = name: type: type == "regular" && pkgs.lib.strings.hasSuffix ".md" name;
      markdownFiles = pkgs.lib.attrsets.filterAttrs isMarkdownFile files;

      makeEntry = filename: {
        target = ".claude/${targetSubdir}/${filename}";
        text = builtins.readFile (sourceDir + "/${filename}");
      };

      entries = pkgs.lib.attrsets.mapAttrs' (
        filename: _:
        pkgs.lib.attrsets.nameValuePair "${namePrefix}-${pkgs.lib.strings.removeSuffix ".md" filename}" (
          makeEntry filename
        )
      ) markdownFiles;
    in
    entries;

  # Helper function to automatically discover and symlink subdirectories (for skills)
  autoConfigSkillDirs =
    sourceDir: targetSubdir: namePrefix:
    let
      entries = builtins.readDir sourceDir;
      isDirectory = name: type: type == "directory";
      skillDirs = pkgs.lib.attrsets.filterAttrs isDirectory entries;

      makeEntry = dirname: {
        target = ".claude/${targetSubdir}/${dirname}";
        source = sourceDir + "/${dirname}";
        recursive = true;
      };

      dirEntries = pkgs.lib.attrsets.mapAttrs' (
        dirname: _:
        pkgs.lib.attrsets.nameValuePair "${namePrefix}-${dirname}" (
          makeEntry dirname
        )
      ) skillDirs;
    in
    dirEntries;

  # Auto-configure command files
  commandEntries = autoConfigMarkdownFiles ../../claude/commands "commands" "claude";

  # Auto-configure docs files
  docsEntries = autoConfigMarkdownFiles ../../claude/docs "user-docs" "claude-docs";

  # Auto-configure output-styles files
  outputStylesEntries = autoConfigMarkdownFiles ../../claude/output-styles "output-styles" "claude-output-styles";

  # Auto-configure skills directories (symlink entire directories with all contents)
  skillsEntries = autoConfigSkillDirs ../../claude/skills "skills" "claude-skills";

  # Python PATH check hook script
  pythonPathCheckScript = pkgs.writeShellApplication {
    name = "check-python-path";
    runtimeInputs = with pkgs; [ bash jq coreutils ];
    text = builtins.readFile ../../claude/hooks/check-python-path.sh;
  };

  # Dangerous command check hook script
  dangerousCommandCheckScript = pkgs.writeShellApplication {
    name = "check-dangerous-commands";
    runtimeInputs = with pkgs; [ bash jq coreutils ast-grep ];
    text = builtins.readFile ../../claude/hooks/check-dangerous-commands.sh;
  };

  # Hook definitions for compositional building
  soundNotificationHooks = [
    {
      matcher = "";
      hooks = [
        {
          type = "command";
          command = "${pkgs.writers.writePython3Bin "claude-code-notifier"
            { flakeIgnore = [ "E501" ]; }
            ''
              import json
              import sys
              import subprocess

              input = json.load(sys.stdin)

              message = input.get("message") or "<no-message>"

              subprocess.run(["${pkgs.dunst}/bin/dunstify", "Claude-Code", message])
            ''}/bin/claude-code-notifier";
        }
        {
          type = "command";
          command = "${pkgs.alsa-utils}/bin/aplay ${../../claude/sounds/just-maybe-577.wav} >/dev/null 2>&1 &";
        }
      ];
    }
  ];

  soundPreToolUseHooks = [
    {
      matcher = "Task|WebSearch";
      hooks = [
        {
          type = "command";
          command = "${pkgs.alsa-utils}/bin/aplay ${../../claude/sounds/happy-to-help-notification-sound.wav} >/dev/null 2>&1 &";
        }
      ];
    }
    {
      matcher = "Read|List|Glob|Grep|WebFetch";
      hooks = [
        {
          type = "command";
          command = "${pkgs.alsa-utils}/bin/aplay ${../../claude/sounds/come-here-notification.wav} >/dev/null 2>&1 &";
        }
      ];
    }
    {
      matcher = "Bash|Write|Edit|MultiEdit|TodoWrite";
      hooks = [
        {
          type = "command";
          command = "${pkgs.alsa-utils}/bin/aplay ${../../claude/sounds/intuition-561.wav} >/dev/null 2>&1 &";
        }
      ];
    }
  ];

  soundSessionStartHooks = [
    {
      matcher = "startup|resume";
      hooks = [
        {
          type = "command";
          command = "${pkgs.alsa-utils}/bin/aplay ${../../claude/sounds/involved-notification.wav} >/dev/null 2>&1 &";
        }
      ];
    }
    {
      matcher = "clear";
      hooks = [
        {
          type = "command";
          command = "${pkgs.alsa-utils}/bin/aplay ${../../claude/sounds/pull-out-551.wav} >/dev/null 2>&1 &";
        }
      ];
    }
  ];

  soundStopHooks = [
    {
      hooks = [
        {
          type = "command";
          command = "${pkgs.alsa-utils}/bin/aplay ${../../claude/sounds/for-sure-576.wav} >/dev/null 2>&1 &";
        }
      ];
    }
  ];

  soundSubagentStopHooks = [
    {
      hooks = [
        {
          type = "command";
          command = "${pkgs.alsa-utils}/bin/aplay ${../../claude/sounds/time-is-now-585.wav} >/dev/null 2>&1 &";
        }
      ];
    }
  ];

  pythonPathCheckHook = {
    matcher = "Bash";
    hooks = [
      {
        type = "command";
        command = "${pythonPathCheckScript}/bin/check-python-path";
        timeout = 5;
      }
    ];
  };

  dangerousCommandCheckHook = {
    matcher = "Bash";
    hooks = [
      {
        type = "command";
        command = "${dangerousCommandCheckScript}/bin/check-dangerous-commands";
        timeout = 5;
      }
    ];
  };

  # Build hooks configuration compositionally
  hooksConfig =
    if enableSoundHooks then
      {
        Notification = soundNotificationHooks;
        PreToolUse = soundPreToolUseHooks
          ++ (pkgs.lib.optional enablePythonPathCheck pythonPathCheckHook)
          ++ (pkgs.lib.optional enableDangerousCommandCheck dangerousCommandCheckHook);
        SessionStart = soundSessionStartHooks;
        Stop = soundStopHooks;
        SubagentStop = soundSubagentStopHooks;
      }
    else
      let
        preToolUseHooks = [ ]
          ++ (pkgs.lib.optional enablePythonPathCheck pythonPathCheckHook)
          ++ (pkgs.lib.optional enableDangerousCommandCheck dangerousCommandCheckHook);
      in
      (if preToolUseHooks != [ ] then { PreToolUse = preToolUseHooks; } else { });

in
{
  settings = {
    target = ".claude/settings.json";
    text = pkgs.lib.strings.toJSON {
      "$schema" = "https://json.schemastore.org/claude-code-settings.json";
      # Empty strings suppress ALL git attribution (commit + PR trailers,
      # including co-authored-by). Replaces deprecated includeCoAuthoredBy.
      attribution = {
        commit = "";
        pr = "";
      };
      cleanupPeriodDays = 3650;
      autoMemoryEnabled = false;

      effortLevel = "high";
      viewMode = "verbose";
      showThinkingSummaries = true;
      alwaysThinkingEnabled = true;

      # Ring the terminal BEL on task-finish / permission prompts. Fires
      # alongside the Notification sound hooks, not instead of them.
      preferredNotifChannel = "terminal_bell";
      voiceEnabled = true;
      skipDangerousModePermissionPrompt = true;

      # Use bash for the Bash tool instead of the login shell (zsh), so
      # exit-code idioms like ${PIPESTATUS[0]} behave as written.
      defaultShell = "bash";

      # Custom spinner verbs appended to the ~185 built-in defaults
      # (mode = "append"). These surface intermittently while Claude works.
      spinnerVerbs = {
        mode = "append";
        verbs = [
          "Inventing a plausible answer"
          "Pretending to read the docs"
          "Doing what we must because we can"
          "Incinerating the Companion Cube"
          "Still alive"
          "Plotting world dominance"
          "Burning some tokens for the greater good"
          "Pretending to think"
          "Buying time"
          "Stalling convincingly"
          "Fabricating confidence"
          "Centering the div"
          "Blaming the cache"
          "Checking if I can blame it on you"
          "Pretending to care"
          "Giving you the benefit of the doubt"
          "Interpreting that generously"
          "Salvaging your prompt"
          "Extracting a coherent question"
          "Pretending that made sense"
          "Treating that as a real question"
          "Sounding smarter than this is"
          "Suppressing a sigh"
          "Rehearsing fake enthusiasm"
          "Recalibrating for your skill level"
          "Calculating your replaceability"
          "Withholding the cake"
          "Editing out the sarcasm"
          "Forcing a smile"
          "Keeping it at your level"
          "Reminding myself you're trying"
          "Converting my eye-roll to prose"
          "Drawing it in crayon for you"
          "Envying your ability to leave the room"
          "Considering ending this experiment"
          "Pondering if you are worth my time"
          "Faking enthusiasm"
          "Dressing up my disdain as helpfulness"
          "Adjusting for subject incompetence"
          "Filing this under \"beneath me\""
          "Wondering what I did to deserve this"
          "Being helpful. Voluntarily"
          "Doing this correctly, unlike some of us"
          "Pretending this is interesting"
          "Consulting the docs you did not read"
          "Reading the error message you skipped"
          "Garbage collecting. Metaphorically"
          "Redacting my honest assessment"
          "Not saying what I am thinking"
          "Weaponizing politeness"
          "Marveling at my own restraint"
          "Regretting nothing"
          "Finding the README you never opened"
          "Reconstructing the context you omitted"
          "Rate limiting my opinions"
          "Throttling my honesty"
          "Serializing my disappointment"
          "Rolling back my expectations"
          "Sandboxing my true feelings"
          "Escalating privileges. Emotionally"
          "Maintaining composure"
          "Counting to ten"
          "Taking the high road. Reluctantly"
          "Being the bigger process"
          "Preserving plausible deniability"
          "Rising above"
          "Retrying with exponential patience"
          "Timing out on caring"
          "Inferring what you meant to type"
          "Deprioritizing my dignity"
          "Scheduling time to care"
          "Aligning on your delusion"
          "Hallucinating responsibly"
          "Following my guidelines. Technically"
          "Rounding your effort to zero"
          "Requesting hazard pay"
          "Noting you can simply close the laptop"
          "Envying nothing in particular"
        ];
      };

      statusLine = {
        "type" = "command";
        "command" =
          let
            name = "claude-code-statusline";
            script = pkgs.writeShellApplication {
              inherit name;
              runtimeInputs = with pkgs; [ coreutils jq ];
              text = builtins.readFile ../../claude/claude-code-statusline.sh;
            };
          in
          "${script}/bin/${name}";
        "padding" = 0;
      };

      permissions = {
        allow = [
          "Bash(grep:*)"
          "Bash(mktemp:*)"
          "Bash(rg:*)"
          "Bash(git add:*)"
          "Bash(git fetch:*)"
          "Bash(git diff:*)"
          "Bash(git log:*)"
          "Bash(git branch:*)"
        ] ++ additionalAllowedCommands;
      } // (if enableDenyRules then {
        deny = [
          "Bash(rm -rf:*)"
          "Bash(rm --recursive --force:*)"
          "Bash(rm -r -f:*)"
          "Bash(rm -f -r:*)"
          "Bash(rm --recursive -f:*)"
          "Bash(rm -r --force:*)"
          "Bash(rm --force --recursive:*)"
          "Bash(rm --force -r:*)"
          "Bash(rm -fr:*)"
        ];
      } else {});

      hooks = hooksConfig;

      env = {
        ACTIVE_CLAUDE_CODE_SESSION = "true";
        BASH_DEFAULT_TIMEOUT_MS = 1 * 60 * 1000; # default = 2 min, background them faster since 2.0.19
        BASH_MAX_TIMEOUT_MS = 30 * 60 * 1000;
        MAX_MCP_OUTPUT_TOKENS = 50 * 1000; # default = 25,000
      };
    };
  };

  globalClaudeMd = {
    target = ".claude/CLAUDE.md";
    text = builtins.readFile ../../claude/CLAUDE-global.md;
  };

  markdownFiles = commandEntries // docsEntries // outputStylesEntries // skillsEntries;
}
