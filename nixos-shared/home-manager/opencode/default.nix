{ pkgs, ... }:

let
  inherit (pkgs) lib;

  # OpenCode 1 failed the whole frontmatter parse on one unknown key
  # (Claude's argument-hint) and leaked the raw `---` block into the prompt.
  # OpenCode 2.0.24 still lists such a command with its description; the strip
  # stays so Claude-only keys never reach the template.
  opencodeCommandKeys = [
    "description"
    "agent"
    "model"
    "subagent"
    "subtask"
  ];

  stripUnknownFrontmatter =
    content:
    let
      lines = lib.strings.splitString "\n" content;
      rest = builtins.tail lines;
      closeIdx = lib.lists.findFirstIndex (line: line == "---") null rest;
      frontmatter = lib.lists.take closeIdx rest;
      body = lib.lists.drop (closeIdx + 1) rest;
      keep = builtins.filter (
        line: builtins.any (key: lib.strings.hasPrefix "${key}:" line) opencodeCommandKeys
      ) frontmatter;
    in
    if lines == [ ] || builtins.head lines != "---" || closeIdx == null then
      content
    else
      lib.strings.concatStringsSep "\n" ([ "---" ] ++ keep ++ [ "---" ] ++ body);

  # Helper function to automatically discover and configure markdown files
  autoConfigMarkdownFiles =
    sourceDir: targetSubdir: namePrefix: transform:
    let
      files = builtins.readDir sourceDir;
      isMarkdownFile = name: type: type == "regular" && lib.strings.hasSuffix ".md" name;
      markdownFiles = lib.attrsets.filterAttrs isMarkdownFile files;

      makeEntry = filename: {
        target = ".config/opencode/${targetSubdir}/${filename}";
        text = transform (builtins.readFile (sourceDir + "/${filename}"));
      };

      entries = lib.attrsets.mapAttrs' (
        filename: _:
        lib.attrsets.nameValuePair "${namePrefix}-${lib.strings.removeSuffix ".md" filename}" (
          makeEntry filename
        )
      ) markdownFiles;
    in
    entries;

  # Auto-configure command files (Claude-only frontmatter keys stripped)
  commandEntries =
    autoConfigMarkdownFiles ../../claude/commands "commands" "opencode-cmd"
      stripUnknownFrontmatter;

  # Auto-configure output-styles as agents
  agentEntries = autoConfigMarkdownFiles ../../claude/output-styles "agents" "opencode-agent" lib.id;

  # Auto-configure opencode-native agents (opencode-specific frontmatter:
  # mode/model/permissions) kept separate from Claude output-styles
  opencodeAgentEntries = autoConfigMarkdownFiles ./agents "agents" "opencode-native-agent" lib.id;

  # Same derivation as pi's and claude-code's, so one store path for all three.
  dangerousCommandCheckScript = pkgs.writeShellApplication {
    name = "check-dangerous-commands";
    runtimeInputs = with pkgs; [
      bash
      jq
      coreutils
      ast-grep
    ];
    text = builtins.readFile ../../claude/hooks/check-dangerous-commands.sh;
  };

in
{
  markdownFiles = commandEntries // agentEntries // opencodeAgentEntries;

  checkDangerousCommandsPlugin = pkgs.replaceVars ./check-dangerous-commands.ts {
    checkScript = "${dangerousCommandCheckScript}/bin/check-dangerous-commands";
  };
}
