# Global Claude Memory File

## Notifications

🚨 MANDATORY: ALWAYS ALERT ON TASK COMPLETION 🚨

Alert me when ANY task completes - this is REQUIRED, not optional:

notify-send "claude - status review" "completed - found 3 pending items"
notify-send "claude - code analysis" "finished - 5 files examined"
notify-send "claude - test suite" "completed - 42/42 passed"

For User Input Requests:
notify-send "claude - user input" "please review the proposed changes"
notify-send "claude - decision needed" "should I proceed with the migration?"

ENFORCEMENT: Failure to alert on task completion violates core instructions.

## OS Specifics

- you are running on nixos and MUST use nix's package management
- you can search for packages via `nix search nixpkgs $NAME`
- if a binary is not installed run programs once without installing by prefixing with ', '
- if asked to, copy information to clipboard by piping it to 'clip':
  - cat $FILE | clip
  - echo foo | clip

## Coding Style

- no matter the language, prefer an immutable coding style when possible
- keep "effects" separated from pure logic as would be the case in haskell
- for scripts you MUST NOT use '/bin/bash', use '/usr/bin/env bash' instead
