{ pkgs, globalMdText, ... }:

let
  # Helper function to automatically discover and configure markdown files
  autoConfigMarkdownFiles =
    sourceDir: targetSubdir: namePrefix:
    let
      files = builtins.readDir sourceDir;
      isMarkdownFile = name: type: type == "regular" && pkgs.lib.strings.hasSuffix ".md" name;
      markdownFiles = pkgs.lib.attrsets.filterAttrs isMarkdownFile files;

      makeEntry = filename: {
        target = ".pi/agent/${targetSubdir}/${filename}";
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

  # Auto-configure command files as prompts
  promptEntries = autoConfigMarkdownFiles
    ../../claude/commands
    "prompts"
    "pi-prompt";

  # Static pi-agent entries
  staticEntries = {
    "pi-agent-global" = {
      target = ".pi/agent/AGENTS.md";
      text = globalMdText;
    };

    "pi-agent-extension-notify" = {
      target = ".pi/agent/extensions/notify.ts";
      text = builtins.readFile ./notify.ts;
    };

    "pi-agent-extension-questionnaire" = {
      target = ".pi/agent/extensions/questionnaire.ts";
      text = builtins.readFile ./questionnaire.ts;
    };

    "pi-agent-extension-sounds" = {
      target = ".pi/agent/extensions/sounds.ts";
      text = builtins.readFile (pkgs.mutate ./sounds.ts {
        aplay = pkgs.alsa-utils;
        sounds = ../../claude/sounds;
      });
    };

    "pi-agent-extension-glados" = {
      target = ".pi/agent/extensions/glados.ts";
      text = builtins.readFile ./glados.ts;
    };

    "pi-agent-extension-todo" = {
      target = ".pi/agent/extensions/todo.ts";
      text = builtins.readFile ./todo.ts;
    };
  };

in
{
  markdownFiles = staticEntries // promptEntries;
}
