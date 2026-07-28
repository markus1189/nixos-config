# Pi Agent Config module

Website of the coding agent: https://github.com/badlogic/pi-mono/tree/main/packages/coding-agent#readme

Documentation: https://github.com/badlogic/pi-mono/tree/main/packages/coding-agent#readme

## Adding a new extension

1. Place the `.ts` file in `extensions/`
2. Register it in `default.nix` under `staticEntries` (between `# START EXTENSIONS` and `# END EXTENSIONS`):
   ```nix
   "pi-agent-extension-<name>" = {
     target = ".pi/agent/extensions/<name>.ts";
     source = ./extensions/<name>.ts;
   };
   ```

## Skills

Pi auto-discovers `~/.agents/skills/` (the harness-neutral location). `default.nix`
links every directory in `../../claude/skills` that contains a `SKILL.md` to
`.agents/skills/<name>`, so the shared skills work in both Claude Code
(`~/.claude/skills`, linked by the `claude-code` module) and pi. No entry in
`~/.pi/agent/settings.json` is needed, which keeps that file fully pi-owned and
writable at runtime.

Consequences:

- A skill directory without a top-level `SKILL.md` is silently skipped.
- Skills installed by hand into `~/.claude/skills` are **not** visible to pi.
  Add them to `../../claude/skills` to make them declarative and shared.
- Pi follows symlinks and de-duplicates by real path, so a skill reachable via
  several configured locations loads once without a collision warning.
