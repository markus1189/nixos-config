---
description: Fetch the review notes I marked in Emacs (annotate.el) and act on them
argument-hint: [--all | --peek]
---

Run `annotate-take $ARGUMENTS` and treat its output as my review.

- One `##` section per comment: where, the text I selected as a
  blockquote, my comment. `## general` is about the change as a whole.
  Address every comment.
- Paths are absolute (`~/…`), not relative to your directory.
- A heading may say the quoted text occurs several times or is no longer
  in the file: then go by the blockquote, not by the line number.
- The call moved these notes out of Emacs (not with `--peek`), so its
  output is the only copy in this session. Never run it again to
  re-read them.
- A closing `Held back in Emacs` line: pass it on to me in one line, and
  do not act on those notes.
- `No annotations to send.`: tell me, and stop.
- Non-zero exit, or no such command: Emacs is not running or predates
  this command. Report the message; do not look for the notes elsewhere.
