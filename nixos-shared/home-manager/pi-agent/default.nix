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

  # Helper function to symlink skill directories into the harness-neutral
  # ~/.agents/skills location, which pi auto-discovers (see pi docs/skills.md).
  # Only directories that actually contain a SKILL.md are linked.
  autoConfigAgentsSkillDirs =
    sourceDir: namePrefix:
    let
      entries = builtins.readDir sourceDir;
      isSkillDir =
        name: type: type == "directory" && builtins.pathExists (sourceDir + "/${name}/SKILL.md");
      skillDirs = pkgs.lib.attrsets.filterAttrs isSkillDir entries;

      makeEntry = dirname: {
        target = ".agents/skills/${dirname}";
        source = sourceDir + "/${dirname}";
        recursive = true;
      };
    in
    pkgs.lib.attrsets.mapAttrs' (
      dirname: _: pkgs.lib.attrsets.nameValuePair "${namePrefix}-${dirname}" (makeEntry dirname)
    ) skillDirs;

  # Auto-configure command files as prompts
  promptEntries = autoConfigMarkdownFiles
    ../../claude/commands
    "prompts"
    "pi-prompt";

  # Expose the shared claude skills to pi via ~/.agents/skills
  agentsSkillEntries = autoConfigAgentsSkillDirs ../../claude/skills "agents-skills";

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
      text = builtins.readFile (pkgs.mutate ./extensions/glados.ts {
        gladosPrompt = builtins.readFile ../../claude/glados-prompt.txt;
      });
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

    "pi-agent-extension-fresh-session" = {
      target = ".pi/agent/extensions/fresh-session.ts";
      source = ./extensions/fresh-session.ts;
    };

    "pi-agent-extension-tokens-per-second" = {
      target = ".pi/agent/extensions/tokens-per-second.ts";
      source = ./extensions/tokens-per-second.ts;
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
  linkedFiles = staticEntries // promptEntries // agentsSkillEntries;
}
