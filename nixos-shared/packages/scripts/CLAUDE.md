# Scripts Package Management

Guide for adding new scripts to this NixOS package collection.

## Script Types

### Shell Scripts (writeShellScript - Custom)
Local helper function defined in this `default.nix`:
```nix
scriptName =
  writeShellScript
    {
      name = "scriptName";
      pure ? true;           # true = only deps in PATH, false = includes system PATH
      deps ? [ ];            # List of nixpkgs dependencies
      failFast ? true;       # Adds 'set -e' to fail on first error
    }
    ''
      # Bash script here
      echo "Hello"
    '';
```

### Shell Applications (writeShellApplication - from nixpkgs)
Includes automatic shellcheck validation:
```nix
scriptName = writeShellApplication {
  name = "scriptName";
  runtimeInputs = [ pkg1 pkg2 ];
  text = ''
    # Bash script here
  '';
};
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
   # Build specific script attribute
   nix-build -E 'let pkgs = import <nixpkgs> {}; in (pkgs.callPackage ./default.nix { markus-wallpapers = "dummy"; })' -A scriptName
   ```
   Many script writers include automatic linting that runs at build time (Python uses flake8, Lua uses luacheck, Fish/Babashka have syntax checks, writeShellApplication uses shellcheck). Build failures indicate linting issues that must be fixed.

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

### Scripts with secrets (pass as function parameters)
```nix
scriptName =
  apiToken:
  writeShellScript
    { name = "scriptName"; deps = [ curl ]; }
    ''
      curl -H "Authorization: Bearer ${apiToken}" ...
    '';
```

### XMobar status output
```bash
echo "<fc=$COLOR>''${TEXT}</fc>"
```

## Troubleshooting

- **Script not found**: Add to `home.packages` in host's `home.nix` or `common-packages.nix`
- **Command not found**: Add package to `deps` list and function parameters
- **pure = true vs false**: Use `true` for reproducibility, `false` if script needs system PATH
- **Missing cacert**: Required for HTTPS curl requests
