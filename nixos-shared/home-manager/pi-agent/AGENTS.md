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
