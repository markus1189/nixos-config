"""Claude Code sound dispatcher: one hook command for every sound event.

Reads the hook JSON on stdin, picks a sound, starts playback detached and
exits. Wired as an async hook, so it never delays a tool call.

AGENT_SOUND_DRY_RUN=1 prints the chosen sound instead of playing it (tests).
"""

import json
import os
import re
import subprocess
import sys

SOUNDS_DIR = "@sounds@"
TIMEOUT = "@timeout@"
APLAY = "@aplay@"

# The vocabulary is shared with pi-agent and opencode (their sounds.ts):
# keep a meaning on the same file across all three.
RESEARCH = "happy-to-help-notification-sound.wav"
READ = "come-here-notification.wav"
MUTATE = "intuition-561.wav"
BUILD = "sly-user-interface-sound.wav"
OUTWARD = "communication-channel-519.wav"
SKILL = "graceful-285.wav"
YOUR_TURN = "your-turn-491.wav"
IDLE = "just-maybe-577.wav"
SESSION_START = "involved-notification.wav"
SESSION_CLEAR = "pull-out-551.wav"
COMPACT = "hollow-582.wav"
STOP = "for-sure-576.wav"
SUBAGENT_STOP = "time-is-now-585.wav"

TOOL_SOUNDS = {
    "Agent": RESEARCH,
    "Task": RESEARCH,
    "WebSearch": RESEARCH,
    "Read": READ,
    "Glob": READ,
    "Grep": READ,
    "WebFetch": READ,
    "Write": MUTATE,
    "Edit": MUTATE,
    "NotebookEdit": MUTATE,
    "TodoWrite": MUTATE,
    "TaskCreate": MUTATE,
    "TaskUpdate": MUTATE,
    "Skill": SKILL,
    # Both block until the user answers.
    "AskUserQuestion": YOUR_TURN,
    "ExitPlanMode": YOUR_TURN,
}
# MCP and every tool not named above: the generic sound, as in pi-agent.
DEFAULT_TOOL_SOUND = RESEARCH

# Notification types where Claude is blocked on the user, not just idle.
BLOCKING_NOTIFICATIONS = {
    "permission_prompt",
    "elicitation_dialog",
    "elicitation_url_dialog",
    "agent_needs_input",
}

SESSION_START_SOUNDS = {
    "startup": SESSION_START,
    "resume": SESSION_START,
    "fork": SESSION_START,
    "clear": SESSION_CLEAR,
    "compact": COMPACT,
}

# ---------------------------------------------------------------------------
# Bash classification. Each segment of a command line gets a class; the
# loudest class wins, so `rg x && git push` is OUTWARD. Unknown programs
# count as MUTATE: that is what every Bash call sounded like before.

NEUTRAL, CLS_READ, CLS_RESEARCH, CLS_BUILD, CLS_MUTATE, CLS_OUTWARD = range(6)
CLASS_SOUNDS = {
    CLS_READ: READ,
    CLS_RESEARCH: RESEARCH,
    CLS_BUILD: BUILD,
    CLS_MUTATE: MUTATE,
    CLS_OUTWARD: OUTWARD,
}

NEUTRAL_PROGRAMS = {
    "cd", "export", "set", "unset", "local", "readonly", "declare", "shift",
    "true", "false", ":", "exit", "return", "sleep", "wait", "trap", "source",
    ".", "pushd", "popd", "printf", "echo", "test", "[", "[[", "]]", "]",
    "break", "continue", "mktemp",
}
READ_PROGRAMS = {
    "ls", "cat", "bat", "head", "tail", "wc", "rg", "grep", "egrep", "fgrep",
    "fd", "find", "tree", "stat", "file", "readlink", "realpath", "basename",
    "dirname", "which", "type", "pwd", "jq", "yq", "sort", "uniq", "cut",
    "tr", "column", "less", "diff", "cmp", "comm", "du", "df", "free", "ps",
    "pgrep", "date", "id", "whoami", "uname", "env", "printenv", "hostname",
    "treemd", "pdftotext", "pdfinfo", "soxi", "ffprobe", "identify",
    "exiftool", "sha256sum", "sha1sum", "md5sum", "b2sum", "xxd", "hexdump",
    "od", "strings", "man", "tldr", "nl", "fold", "fmt", "iconv", "base64",
    "read", "seq", "lsblk", "lsof", "ss", "ip", "nproc", "uptime", "awk",
    "sed", "pandoc", "curl", "wget", "xh", "http", "dig", "host", "nslookup",
    "ping", "journalctl", "nix-store", "nix-instantiate", "pass", "tput",
    "getent", "locale", "col", "expand", "rev", "tac", "zcat", "xzcat", "bzcat", "zipinfo",
}
RESEARCH_PROGRAMS = {"ddgr", "agent-browser", "reddit.py", "w3m", "lynx"}
BUILD_PROGRAMS = {
    "make", "cmake", "ninja", "pytest", "bats", "mvn", "gradle", "gradlew",
    "sbt", "tsc", "nixos-rebuild", "nix-build", "statix", "deadnix",
}
# Programs whose second word decides: {program: {subcommand: class}}.
SUBCOMMANDS = {
    "git": {
        "status": CLS_READ, "log": CLS_READ, "diff": CLS_READ,
        "show": CLS_READ, "branch": CLS_READ, "remote": CLS_READ,
        "rev-parse": CLS_READ, "ls-files": CLS_READ, "blame": CLS_READ,
        "describe": CLS_READ, "grep": CLS_READ, "shortlog": CLS_READ,
        "reflog": CLS_READ, "fetch": CLS_READ, "ls-remote": CLS_READ,
        "cat-file": CLS_READ, "config": CLS_READ, "worktree": CLS_READ,
        "push": CLS_OUTWARD,
    },
    "gh": {
        "pr": CLS_READ, "issue": CLS_READ, "run": CLS_READ, "repo": CLS_READ,
        "search": CLS_RESEARCH, "release": CLS_READ, "api": CLS_READ,
    },
    "nix": {
        "build": CLS_BUILD, "develop": CLS_BUILD, "flake": CLS_BUILD,
        "eval": CLS_READ, "search": CLS_READ, "path-info": CLS_READ,
        "log": CLS_READ, "why-depends": CLS_READ, "store": CLS_READ,
        "derivation": CLS_READ, "hash": CLS_READ,
    },
    "nh": {"os": CLS_BUILD, "home": CLS_BUILD},
    "cargo": {
        "build": CLS_BUILD, "test": CLS_BUILD, "check": CLS_BUILD,
        "clippy": CLS_BUILD, "run": CLS_BUILD,
    },
    "go": {"build": CLS_BUILD, "test": CLS_BUILD, "vet": CLS_BUILD},
    "npm": {
        "install": CLS_BUILD, "ci": CLS_BUILD, "run": CLS_BUILD,
        "test": CLS_BUILD,
    },
    "pnpm": {"install": CLS_BUILD, "run": CLS_BUILD, "test": CLS_BUILD},
    "yarn": {"install": CLS_BUILD, "run": CLS_BUILD, "test": CLS_BUILD},
    "cabal": {"build": CLS_BUILD, "test": CLS_BUILD},
    "stack": {"build": CLS_BUILD, "test": CLS_BUILD},
    "dune": {"build": CLS_BUILD, "test": CLS_BUILD},
    "systemctl": {
        "status": CLS_READ, "show": CLS_READ, "cat": CLS_READ,
        "is-active": CLS_READ, "is-enabled": CLS_READ, "is-failed": CLS_READ,
        "list-units": CLS_READ, "list-timers": CLS_READ,
        "list-unit-files": CLS_READ, "list-dependencies": CLS_READ,
        "show-environment": CLS_READ,
    },
    "tmux": {
        "capture-pane": CLS_READ, "display": CLS_READ,
        "display-message": CLS_READ, "list-panes": CLS_READ,
        "list-windows": CLS_READ, "list-sessions": CLS_READ, "ls": CLS_READ,
        "show-options": CLS_READ,
    },
}
# gh verbs that publish something, whatever the noun: `gh pr create`.
GH_OUTWARD_VERBS = {
    "create", "merge", "comment", "edit", "close", "reopen", "review",
    "delete", "upload", "ready", "lock", "transfer", "rename", "archive",
}
# `gh api` with fields defaults to POST.
GH_API_FIELD_FLAGS = {"-f", "-F", "--field", "--raw-field", "--input"}
# `nix run nixpkgs#ATTR` where the attr isn't the program's name.
PACKAGE_PROGRAMS = {"ripgrep": "rg", "imagemagick": "magick", "fd-find": "fd"}
# Prefixes that run the rest of the line as the real command.
WRAPPERS = {"sudo", "nohup", "exec", "time", "builtin", "xargs",
            "nice", "ionice", "stdbuf", "doas", "then", "do", "else", "elif",
            "if", "while", "until", "!", "{", "}"}
# Their option words that take a value, so the value isn't the program.
WRAPPER_VALUE_OPTS = {"-n", "-I", "-P", "-L", "-d", "-s", "-c", "-k", "-u"}

SHELL_C = re.compile(r"\b(?:ba|z)?sh\s+-c\s+(['\"])(.*?)\1", re.S)
HEREDOC = re.compile(r"<<-?\s*(['\"]?)(\w+)\1")
REDIRECT = re.compile(r"(?:\d|&)?>>?\|?\s*([^\s&|;]+)")
METHOD_FLAG = re.compile(r"^(-X|--request|--method)$")
# POST stays a read: in practice it's search and LLM APIs, not publishing.
WRITE_METHODS = {"PUT", "PATCH", "DELETE"}


def strip_heredocs(command):
    """Drop heredoc bodies: their lines are data, not commands."""
    out, terminator = [], None
    for line in command.split("\n"):
        if terminator is not None:
            if line.strip() == terminator:
                terminator = None
            continue
        out.append(line)
        match = HEREDOC.search(line)
        if match:
            terminator = match.group(2)
    return "\n".join(out)


def mask_quotes(command):
    """Replace quoted text with Q so separators inside quotes don't split."""
    out, quote, i = [], None, 0
    while i < len(command):
        ch = command[i]
        if quote:
            if ch == "\\" and quote == '"':
                i += 2
                continue
            if ch == quote:
                quote = None
                out.append("Q")
        elif ch in "'\"":
            quote = ch
        elif ch == "\\":
            out.append(command[i:i + 2])
            i += 2
            continue
        else:
            out.append(ch)
        i += 1
    return "".join(out)


def split_segments(command):
    text = strip_heredocs(command).replace("\\\n", " ")
    # `bash -c '...'` runs its argument: classify that, not "bash".
    text = SHELL_C.sub(lambda m: "\n" + m.group(2) + "\n", text)
    text = mask_quotes(text)
    text = re.sub(r"\$\{[^}]*\}|\$\(\([^)]*\)\)", "V", text)
    # Command substitutions and subshells hold commands of their own.
    text = re.sub(r"\$\(|[()`]", "\n", text)
    # `2>&1` and `&>` are redirects, not the background operator.
    return re.split(r"\|\||&&|;|\n|\|&?|(?<![>&])&(?![>&])", text)


def unwrap(words):
    """Skip assignments and wrappers like `timeout 5` to the real program."""
    i = 0
    while i < len(words):
        word = words[i]
        if re.match(r"^[A-Za-z_][A-Za-z0-9_]*=", word):
            i += 1
        elif word == "command":
            # `command -v x` looks x up; plain `command x` runs it.
            if words[i + 1:i + 2] in (["-v"], ["-V"]):
                return ["which"] + words[i + 2:]
            i += 1
        elif word == "timeout":
            i += 1
            while i < len(words) and words[i].startswith("-"):
                i += 1
            i += 1  # the duration
        elif word == ",":
            i += 1
        elif word == "nix" and words[i + 1:i + 2] == ["shell"]:
            # nix shell PKGS... --command PROG ARGS
            rest = words[i + 2:]
            for flag in ("--command", "-c"):
                if flag in rest:
                    return unwrap(rest[rest.index(flag) + 1:])
            return words[i:]
        elif word == "nix" and words[i + 1:i + 2] == ["run"]:
            # nix run nixpkgs#PROG -- ARGS: the attr names the program.
            rest = [w for w in words[i + 2:] if not w.startswith("-")]
            if not rest:
                return words[i:]
            program = rest[0].rsplit("#", 1)[-1].rsplit(".", 1)[-1]
            program = PACKAGE_PROGRAMS.get(program, program)
            args = words[words.index("--") + 1:] if "--" in words else []
            return [program] + args
        elif word in WRAPPERS:
            i += 1
            while i < len(words) and words[i].startswith("-"):
                i += 2 if words[i] in WRAPPER_VALUE_OPTS else 1
        else:
            break
    return words[i:]


def http_method(args):
    for i, arg in enumerate(args[:-1]):
        if METHOD_FLAG.match(arg):
            return args[i + 1].upper()
    return None


def classify_curl(args):
    return CLS_OUTWARD if http_method(args) in WRITE_METHODS else CLS_READ


def classify_gh(args):
    if not args:
        return CLS_READ
    if args[0] == "api":
        method = http_method(args)
        if method in WRITE_METHODS or method == "POST":
            return CLS_OUTWARD
        if method is None and any(a in GH_API_FIELD_FLAGS for a in args):
            return CLS_OUTWARD  # fields without -X make gh send a POST
        return CLS_READ
    if len(args) > 1 and args[1] in GH_OUTWARD_VERBS:
        return CLS_OUTWARD
    return SUBCOMMANDS["gh"].get(args[0], CLS_MUTATE)


def classify_segment(segment):
    words = unwrap(segment.split())
    if not words or words[0] in ("for", "case", "esac", "done", "fi", "in"):
        return NEUTRAL
    # Comments, and the rest of a line after a `$(...)` closes.
    if re.match(r"^(#|-|\d*>|&>)", words[0]):
        return NEUTRAL
    program, args = os.path.basename(words[0]), words[1:]
    cls = classify_program(program, args)
    # Writing a file turns any reader into a mutation.
    for target in REDIRECT.findall(" ".join(args)):
        if not target.startswith("/dev/"):
            cls = max(cls, CLS_MUTATE)
    return cls


def first_subcommand(args):
    """First non-option word, skipping values of `git -C dir`-style options."""
    i = 0
    while i < len(args):
        if args[i] in ("-C", "-c", "--git-dir", "--work-tree", "--repo", "-R"):
            i += 2
        elif args[i].startswith("-"):
            i += 1
        else:
            return args[i]
    return ""


def classify_program(program, args):
    if program in NEUTRAL_PROGRAMS:
        return NEUTRAL
    if program in ("curl", "xh", "http"):
        return classify_curl(args)
    if program == "gh":
        return classify_gh(args)
    if program == "sed" and any(a.startswith("-i") for a in args):
        return CLS_MUTATE
    if program in SUBCOMMANDS:
        sub = first_subcommand(args)
        return SUBCOMMANDS[program].get(sub, CLS_MUTATE)
    if program in READ_PROGRAMS:
        return CLS_READ
    if program in RESEARCH_PROGRAMS:
        return CLS_RESEARCH
    if program in BUILD_PROGRAMS:
        return CLS_BUILD
    return CLS_MUTATE


def classify_bash(command):
    cls = max((classify_segment(s) for s in split_segments(command)),
              default=NEUTRAL)
    return CLASS_SOUNDS.get(cls, MUTATE)


# ---------------------------------------------------------------------------


def choose_sound(event):
    name = event.get("hook_event_name")
    if name == "PreToolUse":
        tool = event.get("tool_name") or ""
        if tool == "Bash":
            return classify_bash((event.get("tool_input") or {}).get("command") or "")
        return TOOL_SOUNDS.get(tool, DEFAULT_TOOL_SOUND)
    if name == "Notification":
        if event.get("notification_type") in BLOCKING_NOTIFICATIONS:
            return YOUR_TURN
        return IDLE
    if name == "SessionStart":
        return SESSION_START_SOUNDS.get(event.get("source"))
    if name == "Stop":
        return STOP
    if name == "SubagentStop":
        return SUBAGENT_STOP
    return None


def play(sound):
    # The timeout is load-bearing: if the audio stack wedges, aplay blocks
    # forever on the PipeWire socket and every tool call leaks a process.
    subprocess.Popen(
        [TIMEOUT, "5", APLAY, os.path.join(SOUNDS_DIR, sound)],
        stdin=subprocess.DEVNULL,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        start_new_session=True,
    )


def main():
    try:
        event = json.load(sys.stdin)
    except ValueError:
        return
    sound = choose_sound(event)
    if os.environ.get("AGENT_SOUND_DRY_RUN"):
        print(sound or "")
    elif sound:
        play(sound)


if __name__ == "__main__":
    main()
