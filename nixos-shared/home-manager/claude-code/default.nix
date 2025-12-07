{ pkgs
, enableSoundHooks ? false
, enableDenyRules ? false
, enablePythonPathCheck ? false
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

  # Auto-configure agent files
  agentEntries = autoConfigMarkdownFiles ../../claude/agents "agents" "claude";

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

in
{
  settings = {
    target = ".claude/settings.json";
    text = pkgs.lib.strings.toJSON {
      includeCoAuthoredBy = false;
      cleanupPeriodDays = 3650;

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

      hooks = (if enableSoundHooks then {
        Notification = [
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
        PreToolUse = [
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
        ] ++ (if enablePythonPathCheck then [
          {
            matcher = "Bash";
            hooks = [
              {
                type = "command";
                command = "${pythonPathCheckScript}/bin/check-python-path";
                timeout = 5;
              }
            ];
          }
        ] else []);
        SessionStart = [
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
        Stop = [
          {
            hooks = [
              {
                type = "command";
                command = "${pkgs.alsa-utils}/bin/aplay ${../../claude/sounds/for-sure-576.wav} >/dev/null 2>&1 &";
              }
            ];
          }
        ];
        SubagentStop = [
          {
            hooks = [
              {
                type = "command";
                command = "${pkgs.alsa-utils}/bin/aplay ${../../claude/sounds/time-is-now-585.wav} >/dev/null 2>&1 &";
              }
            ];
          }
        ];
      } else if enablePythonPathCheck then {
        PreToolUse = [
          {
            matcher = "Bash";
            hooks = [
              {
                type = "command";
                command = "${pythonPathCheckScript}/bin/check-python-path";
                timeout = 5;
              }
            ];
          }
        ];
      } else {});

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

  markdownFiles = commandEntries // agentEntries // docsEntries // outputStylesEntries // skillsEntries;
}
