# Syncthing

Mesh: p1g8, nixos-p1, nuc (send-receive) + phone S26U. Declared in
`nixos-shared/syncthing.nix`; module quirks are in its header comment.

## Rollout

- nuc builds from `github:…/nixos-config`: push first, then
  `nh os switch -- --refresh` (tarball cache 1h, else it rebuilds the old rev).
- New folder on S26U: the phone accepts the offer by hand (path, folder type).
  Nothing on the phone is declarative.
- Verify on every host: `syncthing-init` Result `success`, `.stignore` equals
  `nix eval …folders.<name>.ignorePatterns`, folder `idle`, 0 `folder/errors`.

## `~/Stuff` patterns (`stuff-patterns.nix`)

Opt-in: per-host names, then `junkDirs`, then `*.ext/**`, then `!*.ext`, then `*`.
First match wins. Each rule below cost a bug:

- A pattern matching a dir applies to everything below it: `!*.org` let
  `repo.gradle.org/*.xml` through. Hence `*.ext/**` before the negations.
  Such dirs still sync, empty, and a remote delete of one fails with a pull
  error while it holds ignored files. Don't `(?d)`: it would delete local scratch.
- Junk before the first `!`: otherwise Syncthing walks into node_modules and
  its `.md` match.
- Other dirs are ignored by `*` (not indexed): a remote delete removes only the
  synced files; local-only files and the dir stay, no error.
- Emacs lock files `.#x.md` are symlinks: ignored by `.#*`.

## Testing a pattern change (no switch)

Throwaway folder on two hosts via REST (key: `<apikey>` in
`~/.config/syncthing/config.xml`, GUI `127.0.0.1:8384`):

1. Seed a dir with a real month (`rsync --max-size=1M`) plus traps.
2. `POST /rest/config/folders` `{id, path, paused:true, devices:[{deviceID:<peer>}]}`
   on both, `POST /rest/db/ignores?folder=<id>` `{"ignore":[…]}` with the
   `nix eval --json` list, then `PATCH …/folders/<id>` `{"paused":false}`.
3. Diff the peer's received files against an expected list computed
   independently (not by Syncthing). Must be identical.
4. `DELETE /rest/config/folders/<id>` on both, remove the dirs.

## Checks (REST, per host)

- `GET /rest/db/status?folder=<id>`: state, need, errors
- `GET /rest/folder/errors?folder=<id>`: failed items
- `GET /rest/db/completion?folder=<id>&device=<ID>`: a peer's progress
- `GET /rest/cluster/pending/devices|folders`: what the GUI would pop up;
  dismiss a device with `DELETE /rest/cluster/pending/devices?device=<ID>`
  (`remoteIgnoredDevices` has no REST endpoint)

## Phone (S26U)

App `com.nutomic.syncthingandroid` (discontinued). Its web API needs the app's
key, so drive and verify over adb (`nixpkgs#android-tools`, wireless debugging;
port changes).

- Compare files, not labels: the app shows a greyed "Send Only" that is not
  the type, and hosts' `completion` for it has been wrong.
- Storage is case-insensitive: `a.py` next to `A.py` fails on the phone.
- No symlinks: replace with files (`CLAUDE.md` → `@AGENTS.md`).
- `stuff` is Receive Only there. Before switching it to Send & Receive,
  revert its local changes, or they are sent to every host.
