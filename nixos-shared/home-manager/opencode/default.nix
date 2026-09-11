{ pkgs, ... }:

let
  inherit (pkgs) lib;

  # opencode's command frontmatter schema only knows description/agent/model/
  # subtask. A single unknown key (Claude's argument-hint) fails the whole
  # parse, and opencode then keeps the raw `---` block as part of the prompt
  # and shows no description. So instead of skipping every command that has
  # frontmatter, strip it down to the keys opencode understands.
  opencodeCommandKeys = [
    "description"
    "agent"
    "model"
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
    autoConfigMarkdownFiles ../../claude/commands "command" "opencode-cmd"
      stripUnknownFrontmatter;

  # Auto-configure output-styles as agents
  agentEntries = autoConfigMarkdownFiles ../../claude/output-styles "agent" "opencode-agent" lib.id;

  # Auto-configure opencode-native agents (opencode-specific frontmatter:
  # mode/model/permission/temperature) kept separate from Claude output-styles
  opencodeAgentEntries = autoConfigMarkdownFiles ./agents "agents" "opencode-native-agent" lib.id;

in
{
  markdownFiles = commandEntries // agentEntries // opencodeAgentEntries;
}
