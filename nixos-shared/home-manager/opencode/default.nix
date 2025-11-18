{ pkgs, ... }:

let
  # Helper function to check if a file starts with YAML frontmatter
  hasYamlFrontmatter = filePath:
    let
      content = builtins.readFile filePath;
      lines = pkgs.lib.strings.splitString "\n" content;
      firstLine = builtins.head lines;
    in
    firstLine == "---";

  # Helper function to automatically discover and configure markdown files
  autoConfigMarkdownFiles =
    sourceDir: targetSubdir: namePrefix: filterFn:
    let
      files = builtins.readDir sourceDir;
      isMarkdownFile = name: type: type == "regular" && pkgs.lib.strings.hasSuffix ".md" name;
      markdownFiles = pkgs.lib.attrsets.filterAttrs isMarkdownFile files;

      # Apply additional filter function
      filteredFiles = pkgs.lib.attrsets.filterAttrs
        (filename: _: filterFn (sourceDir + "/${filename}"))
        markdownFiles;

      makeEntry = filename: {
        target = ".config/opencode/${targetSubdir}/${filename}";
        text = builtins.readFile (sourceDir + "/${filename}");
      };

      entries = pkgs.lib.attrsets.mapAttrs' (
        filename: _:
        pkgs.lib.attrsets.nameValuePair "${namePrefix}-${pkgs.lib.strings.removeSuffix ".md" filename}" (
          makeEntry filename
        )
      ) filteredFiles;
    in
    entries;

  # Auto-configure command files (exclude files with YAML frontmatter)
  commandEntries = autoConfigMarkdownFiles
    ../../claude/commands
    "command"
    "opencode-cmd"
    (filePath: !(hasYamlFrontmatter filePath));

  # Auto-configure output-styles as agents (no filter needed)
  agentEntries = autoConfigMarkdownFiles
    ../../claude/output-styles
    "agent"
    "opencode-agent"
    (_: true);

in
{
  markdownFiles = commandEntries // agentEntries;
}
