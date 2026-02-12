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

    # START EXTENSIONS

    "pi-agent-extension-notify" = {
      target = ".pi/agent/extensions/notify.ts";
      text = builtins.readFile ./extensions/notify.ts;
    };

    "pi-agent-extension-questionnaire" = {
      target = ".pi/agent/extensions/questionnaire.ts";
      text = builtins.readFile ./extensions/questionnaire.ts;
    };

    "pi-agent-extension-sounds" = {
      target = ".pi/agent/extensions/sounds.ts";
      text = builtins.readFile (pkgs.mutate ./extensions/sounds.ts {
        aplay = pkgs.alsa-utils;
        sounds = ../../claude/sounds;
      });
    };

    "pi-agent-extension-glados" = {
      target = ".pi/agent/extensions/glados.ts";
      text = builtins.readFile ./extensions/glados.ts;
    };

    "pi-agent-extension-block-dangerous" = {
      target = ".pi/agent/extensions/block-dangerous.ts";
      text = builtins.readFile ./extensions/block-dangerous.ts;
    };

    "pi-agent-extension-web-tools" = {
      target = ".pi/agent/extensions/web-tools.ts";
      source = ./extensions/web-tools.ts;
    };

    "pi-agent-extension-qna" = {
      target = ".pi/agent/extensions/qna.ts";
      source = ./extensions/qna.ts;
    };

    "pi-agent-extension-instruction-resolver" = {
      target = ".pi/agent/extensions/instruction-resolver.ts";
      source = ./extensions/instruction-resolver.ts;
    };

    "pi-agent-extension-think" = {
      target = ".pi/agent/extensions/think.ts";
      source = ./extensions/think.ts;
    };

    # END EXTENSIONS

    "pi-agent-models" = {
      target = ".pi/agent/models.json";
      text = builtins.readFile ./models.json;
    };

    "pi-agent-keybindings" = {
      target = ".pi/agent/keybindings.json";
      source = ./keybindings.json;
    };

  };

in
{
  linkedFiles = staticEntries // promptEntries;
}
