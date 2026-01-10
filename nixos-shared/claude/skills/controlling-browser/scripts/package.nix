# Nix package for web-browser-skill dependencies
{ pkgs ? import <nixpkgs> {} }:

let
  nodeEnv = pkgs.callPackage "${pkgs.path}/pkgs/development/node-packages/node-env.nix" {
    inherit (pkgs) stdenv lib python2 runCommand writeTextFile writeShellScript;
    inherit pkgs nodejs;
    libtool = if pkgs.stdenv.isDarwin then pkgs.darwin.cctools else null;
  };

  # Create a simple package.json and package-lock.json for ws dependency
  packageJson = pkgs.writeText "package.json" ''
    {
      "name": "web-browser-skill",
      "version": "1.0.0",
      "type": "module",
      "dependencies": {
        "ws": "^8.18.0"
      }
    }
  '';

in pkgs.mkShell {
  buildInputs = [
    pkgs.nodejs_22
    pkgs.chromium
    pkgs.rsync
  ];

  shellHook = ''
    # Create node_modules if it doesn't exist
    if [ ! -d "node_modules" ]; then
      ${pkgs.nodejs_22}/bin/npm install ws@8.18.0
    fi
  '';
}
