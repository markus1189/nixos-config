# Scripts Package Management

Guide for adding new scripts to this NixOS package collection.

## Script Types

### Shell Scripts (writeShellApplication - from nixpkgs)
The only shell writer used here. shellcheck runs at build time, so a lint is a
build failure:
```nix
scriptName = writeShellApplication {
  name = "scriptName";
  runtimeInputs = [ pkg1 pkg2 ];   # prepended to PATH
  inheritPath = false;             # false = only runtimeInputs on PATH
  bashOptions = [ "errexit" ];     # default is errexit+nounset+pipefail
  text = ''
    # Bash script here
  '';
};
```
`bashOptions` is spelled out on the scripts converted from the old local
`writeShellScript` helper, which only ever set `set -e`; new scripts are better
off with the default (`nounset` and `pipefail` included) unless they genuinely
need to read unset variables.

Where a lint is deliberate — word splitting that is the point, or sourcing a
runtime file such as `/run/agenix/telegram.env` that does not exist at build
time — annotate the line rather than loosening the whole build:
```bash
# shellcheck source=/dev/null
. /run/agenix/telegram.env

# shellcheck disable=SC2046
jo -a $(...)
```

### Python Scripts (writers.writePython3Bin)
```nix
scriptName = writers.writePython3Bin "scriptName" {
  libraries = [ python3Packages.requests ];
} ''
  import requests
  # Python code here
'';
```

Or with external file:
```nix
scriptName = writers.writePython3Bin "scriptName" { } (builtins.readFile ./scriptName.py);
```

### Other Writers from nixpkgs

**Available writers:**
- `writers.writeBash`, `writeBashBin` - Bash scripts
- `writers.writeDash`, `writeDashBin` - Dash scripts
- `writers.writeFish`, `writeFishBin` - Fish shell scripts
- `writers.writeNu`, `writeNuBin` - Nushell scripts
- `writers.writePython3`, `writePython3Bin` - Python 3 scripts
- `writers.writePyPy2`, `writePyPy2Bin` - PyPy2 scripts
- `writers.writePyPy3`, `writePyPy3Bin` - PyPy3 scripts
- `writers.writePerl`, `writePerlBin` - Perl scripts
- `writers.writeRuby`, `writeRubyBin` - Ruby scripts
- `writers.writeLua`, `writeLuaBin` - Lua scripts
- `writers.writeHaskell`, `writeHaskellBin` - Haskell programs
- `writers.writeRust`, `writeRustBin` - Rust programs
- `writers.writeNim`, `writeNimBin` - Nim programs
- `writers.writeJS`, `writeJSBin` - JavaScript (Node.js) scripts
- `writers.writeFSharp`, `writeFSharpBin` - F# scripts
- `writers.writeGuile`, `writeGuileBin` - Guile Scheme scripts
- `writers.writeBabashka`, `writeBabashkaBin` - Babashka (Clojure) scripts

All `*Bin` variants create scripts in `/bin/` subdirectory.

## Adding a New Script

1. **Add dependencies to function parameters** (top of `default.nix`, lines 1-62)

2. **Add script to `rec { }` block** in `default.nix`

3. **Validate syntax:**
   ```bash
   nix-instantiate --parse default.nix
   ```

4. **Run linters/checks** (build the script to run automatic linters):
   ```bash
   # Build a specific script via the flake output (run from the repo root;
   # git add new files first — untracked files don't exist for flake eval)
   nix build .#myScripts.scriptName

   # Or run it directly
   nix run .#myScripts.scriptName
   ```
   This builds against the repo's pinned nixpkgs with the hosts' overlays — the same derivations the hosts install. Many script writers include automatic linting that runs at build time (Python uses flake8, Lua uses luacheck, Fish/Babashka have syntax checks, writeShellApplication uses shellcheck). Build failures indicate linting issues that must be fixed.

5. **Commit:**
   ```bash
   git add default.nix
   git commit -m "scripts: add scriptName"
   ```

## Commit Message Patterns

- `scripts: add scriptName` - New script
- `scripts: improve scriptName` - Enhancement
- `scripts: fix scriptName` - Bug fix
- `scripts: format and improve 'scriptName'` - Formatting + changes

## Common Patterns

### API calls with HTTPS
```bash
curl --cacert ${cacert}/etc/ssl/certs/ca-bundle.crt \
  --url https://api.example.com
```

### Retry logic
```bash
unset c
until RESULT="$(command)" && [[ ! -z "$RESULT" ]]; do
    ((c++)) && ((c==10)) && break
    sleep 3
done
```

### Scripts with secrets (read /run/agenix at runtime)
Secrets are agenix runtime secrets — never bake them into the store or pass
them as constructor parameters. Read them from `/run/agenix/<name>` when the
script runs:
```bash
curl -H "Authorization: Bearer $(< /run/agenix/apiToken)" ...
# or for env-style secrets:
. /run/agenix/telegram.env
```
Declarations live in `nixos-shared/runtime-secrets.nix` / per-module
`age.secrets`; see the "Secrets Management" section in the repo AGENTS.md.

### XMobar status output
```bash
echo "<fc=$COLOR>''${TEXT}</fc>"
```

## Troubleshooting

- **Script not found**: Add to `home.packages` in host's `home.nix` or `common-packages.nix`
- **Command not found**: Add package to `runtimeInputs` and to the function parameters
- **inheritPath**: `false` for reproducibility, `true` if the script needs the caller's PATH
- **Missing cacert**: Required for HTTPS curl requests
