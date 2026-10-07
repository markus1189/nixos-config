---
name: sourcegraph-search
description: Searches public code across many repositories on sourcegraph.com with the `src` CLI. Use for "how do others implement X", real-world usage of an API or function across public repos, finding code patterns, or searching commits and diffs of public projects.
---

# Sourcegraph code search

`src search '<query>'` queries sourcegraph.com (public code). Anonymous use
works; `SRC_ACCESS_TOKEN` raises limits (anonymous rate limit: unverified).

## Query grammar

- **Content patterns are literal.** `mkDeriv.*on` matches nothing; wrap in
  slashes, `/mkDeriv.*on/`, or add `patterntype:regexp` for the whole query.
- `repo:`, `file:`, `lang:` take regexes, unanchored: `repo:^github\.com/NixOS/nixpkgs$`,
  `file:\.nix$`, `lang:go`. Repo names carry the host, and only repos that
  sourcegraph.com indexes match: `repo:facebook/react` printed "No repositories
  found".
- `count:N` caps results; always set it (`count:3` to explore).
- `-file:test` / `-repo:...` negate; put them after a positive term (a leading
  `-` is parsed as a flag; use `src search -- '-file:x foo'`).
- `type:diff` / `type:commit` search history (with `after:"1 week ago"`,
  `author:`); `select:repo` lists matching repos only; `fork:no archived:no`
  drop noise (slow on broad queries: one took 30s).
- `type:symbol` printed only a file header (`0 matches`), so prefer content
  search for definitions.

## Output

- Reading: plain output (1.2 KB for the `count:3` regex query below).
- Parsing: `src search -stream -json '...'` (2.1 KB for the same query).
  Never bare `-json`: it embeds whole file contents (22.9 KB here; 106 KB vs
  488 bytes plain for another `count:2` query).

## Verified examples

Each returned 3 results (the regex example returned 3+; `select:repo` lists 3 repos).

```bash
# literal word, restricted to one directory of a repo
src search 'repo:^github\.com/NixOS/nixpkgs$ lib.mkIf file:nixos/modules/services/web-servers count:3'
# regex content patterns
src search 'lang:nix /mkDerivation.*rec/ count:3'
src search 'lang:go -file:_test /func \(.*\) ServeHTTP/ count:3'
# which repos use an API
src search 'lang:nix writeShellApplication -file:test count:3 select:repo'
# commits and recent diffs
src search 'type:commit repo:^github\.com/NixOS/nixpkgs$ systemd count:3'
src search 'type:diff repo:^github\.com/NixOS/nixpkgs$ after:"1 week ago" patterntype:regexp mkIf.*enable count:3'
```
