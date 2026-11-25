"""Daily digest of this host's Claude Code sessions, from the transcripts.

session-digest --write --host TAG
    (over)write ~/Stuff/YYYY-MM/DD-TAG/autogen-session-digest.md for today,
    yesterday and the start day of every session changed since yesterday,
    each only if its content changed
session-digest [--date D | --days N]
    print one day (default today) or the last N days
"""

import argparse
import collections
import functools
import json
import os
import re
import subprocess
import sys
from datetime import date, datetime, time, timedelta
from pathlib import Path

import numpy as np

# Replaced with a store path by the Nix build.
GIT = "@git@"
if GIT.startswith("@"):
    GIT = "git"

FILE_NAME = "autogen-session-digest.md"
IDLE = timedelta(
    minutes=5
)  # longer gaps between records count as away, not working
ROLLUP_MIN = (
    5  # a day with fewer sessions gets no project/cost/recurring tables
)
ACK = re.compile(
    r"^(ok(ay)?|yes|ja|jo|y|next|weiter|go( on)?|approved?|continue|do it"
    r"|passt|lgtm|works"
    r"|perfect|thanks?|danke|nice|good)\b\W*$",
    re.I,
)
# Built-in commands that set up the session rather than say what it is about.
SETUP = {
    "/model",
    "/clear",
    "/output-style",
    "/effort",
    "/fast",
    "/config",
    "/resume",
    "/compact",
    "/login",
    "/mcp",
    "/permissions",
    "/cost",
    "/status",
    "/context",
    "/rename",
    "/color",
}
SLASH = re.compile(
    r"<command-name>(.*?)</command-name>.*?<command-args>(.*?)</command-args>",
    re.S,
)
PASTE = re.compile(r"<pasted_content[^>]*>.*?(</pasted_content>|$)", re.S)
MARGINAL = "My annotations on your last message, from marginal"
FILLER = re.compile(
    r"^(I've now|I have now|I now|Now I|Ich habe jetzt|Jetzt|Gern"
    r"|You're welcome"
    r"|Du hast also|Let me|Okay|OK|Got it|Alright|Sure)\b",
    re.I,
)
COMMIT_OUT = re.compile(
    r"^\[[^\]\s]+ ([0-9a-f]{7,})\] (.*)$", re.M
)  # `git commit` stdout; -q prints none
GIT_COMMIT = re.compile(
    r"\bgit\b(?:\s+-C\s+(\S+))?[^|;&]*?\scommit\b(?![^|;&]*--dry-run)"
)
SUBJECT = re.compile(
    r"""\s-[a-zA-Z]*(?:m\s*(?:"\$\(cat <<'?\w+'?\n(?P<h>.*?)\n"""
    r"""|"(?P<d>(?:[^"\\]|\\.)*)"|'(?P<s>[^']*)')"""
    r"""|F\s*-\s*<<-?'?\w+'?\n(?P<f>.*?)\n)""",
    re.S,
)
ARTIFACT_URL = "https://claude.ai/artifact/"
DOCS = "mcp__claude_ai_Claude_Docs__"
# Docs address a doc by its frame slug (a UUID) or by the id in its URL.
UUID = re.compile(r"^[0-9a-f]{8}-[0-9a-f]{4}-")
# (key, per-response usage field, cost-state modelUsage field)
TOKEN_FIELDS = (
    ("in", "input_tokens", "inputTokens"),
    ("out", "output_tokens", "outputTokens"),
    ("cache_read", "cache_read_input_tokens", "cacheReadInputTokens"),
    ("cache_write", "cache_creation_input_tokens", "cacheCreationInputTokens"),
)


# --- transcript --------------------------------------------------------------


def records(path):
    with open(path, errors="replace") as f:
        for line in f:
            try:
                yield json.loads(line)
            except ValueError:
                pass


def when(ts):
    return datetime.fromisoformat(ts.replace("Z", "+00:00")).astimezone()


def text_of(content):
    if isinstance(content, str):
        return content
    return "\n".join(
        b.get("text", "")
        for b in content or []
        if isinstance(b, dict) and b.get("type") == "text"
    )


def oneline(s):
    return " ".join(s.split())


def prompt_text(raw):
    """What the user typed, or None for injected context and setup commands."""
    txt = raw.strip()
    if m := SLASH.search(txt):
        txt = f"{m[1]} {m[2]}".strip()
    if not txt or txt.startswith("<"):
        return None
    if txt.split()[0] in SETUP:
        return None
    if txt.startswith(MARGINAL):
        return "[marginal annotations]"
    return oneline(PASTE.sub("[paste] ", txt))


def commit_intent(cmd, cwd):
    """(repo dir, subject) if the shell command runs `git commit`."""
    m = GIT_COMMIT.search(cmd)
    if not m:
        return None
    cd = re.match(r"\s*cd\s+(\S+)\s*&&", cmd)
    repo = (m[1] or (cd and cd[1]) or cwd).strip("\"'")
    if repo.startswith("~"):
        repo = str(Path.home()) + repo[1:]
    sm = SUBJECT.search(cmd, max(m.end() - 7, 0))
    subject = next(
        (g for g in (sm.group("h", "d", "s", "f") if sm else ()) if g), ""
    ).strip()
    return repo, subject.splitlines()[0] if subject else ""


def hash_in_output(out, subject):
    if m := COMMIT_OUT.search(out):
        return m[1][:7], m[2]
    if subject:  # `git commit -q && git log --oneline -1` and friends
        m = re.search(
            rf"^([0-9a-f]{{7,40}}) {re.escape(subject[:40])}", out, re.M
        )
        if m:
            return m[1][:7], subject
    return None


def parse(path):
    """What the digest shows about one session; None without a typed prompt."""
    s = dict(
        id=path.stem,
        title=None,
        cwd=None,
        branch=None,
        cost=None,
        model_usage=None,
        headless=False,
        prompts=[],
        replies=[],
        last_kind=None,
        files=[],
        tools=collections.Counter(),
        usage={},
        ts=[],
        raw_commits=[],
        artifacts={},
        docs={},
        doc_urls={},
        agents=len(
            list((path.parent / path.stem / "subagents").glob("*.jsonl"))
        ),
    )
    pending, calls = {}, {}
    for r in records(path):
        t = r.get("type")
        if r.get("timestamp"):
            s["ts"].append(when(r["timestamp"]))
        if t == "ai-title":
            s["title"] = r.get("aiTitle") or s["title"]
        elif t == "cost-state":
            s["cost"] = r.get("totalCostUSD", s["cost"])
            s["model_usage"] = r.get("modelUsage") or s["model_usage"]
        if t not in ("user", "assistant") or r.get("isSidechain"):
            continue
        # promptSource 'sdk' also marks messages sent into an interactive
        # session: not a signal.
        s["headless"] |= r.get("entrypoint") == "sdk-cli"
        s["cwd"] = r.get("cwd") or s["cwd"]
        s["branch"] = r.get("gitBranch") or s["branch"]
        msg = r.get("message") or {}
        if t == "assistant" and msg.get("usage"):
            s["usage"][msg.get("id")] = (
                msg.get("model"),
                msg["usage"],
            )  # one response spans several records
        content = msg.get("content")
        for b in content if isinstance(content, list) else []:
            if not isinstance(b, dict):
                continue
            if b.get("type") == "tool_use":
                name, inp = b.get("name"), b.get("input") or {}
                s["tools"][name] += 1
                fp = (
                    inp.get("file_path")
                    if name in ("Edit", "Write", "NotebookEdit")
                    else None
                )
                if fp and fp not in s["files"]:
                    s["files"].append(fp)
                if name == "Bash" and (
                    ci := commit_intent(
                        inp.get("command", ""), s["cwd"] or "."
                    )
                ):
                    pending[b.get("id")] = ci
                elif name == "Artifact" or name.startswith(DOCS):
                    calls[b.get("id")] = (name, inp)
            elif b.get("type") == "tool_result" and (
                call := calls.pop(b.get("tool_use_id"), None)
            ):
                track_artifact(s, *call, b, r)
            elif b.get("type") == "tool_result" and (
                ci := pending.pop(b.get("tool_use_id"), None)
            ):
                hit = hash_in_output(text_of(b.get("content")), ci[1])
                if hit or ci[1]:
                    s["raw_commits"].append(
                        (
                            hit[0] if hit else None,
                            hit[1] if hit else ci[1],
                            ci[0],
                        )
                    )
        if r.get("isMeta"):
            continue
        if t == "user":
            if (p := prompt_text(text_of(content))) is not None:
                s["prompts"].append(p)
                s["last_kind"] = "user"
        elif txt := text_of(content).strip():
            s["replies"].append(txt)
            s["last_kind"] = "assistant"
    if not s["ts"] or not s["prompts"]:
        return None
    resolve_docs(s)
    s["ts"].sort()
    s["start"] = s["ts"][0]
    gaps = (b - a for a, b in zip(s["ts"], s["ts"][1:]))
    s["active"] = (
        sum((g for g in gaps if g < IDLE), timedelta()).total_seconds() / 60
    )
    s["tokens"] = tokens(s)
    s["project"] = project(s["cwd"]) if s["cwd"] else path.parent.name
    return s


def docs_result(content):
    """The JSON object a Claude Docs result holds, past any notice line."""
    txt = text_of(content)
    try:
        d = json.JSONDecoder().raw_decode(txt, txt.index("{"))[0]
    except ValueError:
        return {}
    return d if isinstance(d, dict) else {}


def track_artifact(s, name, inp, result, record):
    """Pages published and docs created or edited. Any Docs result with a
    frame, reads included, maps that doc's ids to its URL."""
    if result.get("is_error"):
        return
    if name == "Artifact":
        tur = record.get("toolUseResult")
        if (
            inp.get("action") not in (None, "publish")
            or inp.get("asset")
            or not isinstance(tur, dict)
            or not tur.get("url")
        ):
            return
        a = s["artifacts"].setdefault(
            tur["url"],
            dict(kind="page", url=tur["url"], title=None, new=False, v=None),
        )
        a["title"] = tur.get("title") or a["title"]
        # A page made from a type has no seq; its first file publish
        # already says updated.
        a["new"] |= tur.get("updated") is False or bool(
            tur.get("created_from_type")
        )
        a["v"] = tur.get("seq") or a["v"]
        return
    d = docs_result(result.get("content"))
    if d.get("verdict") != "allow":
        return
    frame = d.get("frame") or {}
    if url := frame.get("url"):
        s["doc_urls"][url.rsplit("/", 1)[-1]] = url
        if frame.get("slug"):
            s["doc_urls"][frame["slug"]] = url
    container = inp.get("container") or {}
    tool = name.removeprefix(DOCS)
    if tool == "batch" and "create" in container:
        key, title = frame.get("slug"), container["create"].get("name")
    elif tool in ("batch", "update") and container.get("id"):
        key, title = container["id"], None
    else:
        return
    if not key:
        return
    doc = s["docs"].setdefault(key, dict(title=None, new=False, v=None))
    doc["title"] = title or doc["title"]
    doc["new"] |= title is not None
    doc["v"] = d.get("rev") or doc["v"]


def resolve_docs(s):
    """Docs into s['artifacts'] by URL: edits name a doc by slug or by its
    URL's id, and the frame that maps a slug may come after the edit."""
    urls = s.pop("doc_urls")
    for key, doc in s.pop("docs").items():
        url = urls.get(key) or (
            None if UUID.match(key) else ARTIFACT_URL + key
        )
        a = s["artifacts"].setdefault(
            url or key,
            dict(kind="doc", url=url, id=key, title=None, new=False, v=None),
        )
        a["title"] = a["title"] or doc["title"]
        a["new"] |= doc["new"]
        a["v"] = max(filter(None, (a["v"], doc["v"])), default=None)


def tokens(s):
    # cost-state also counts side calls (titles, subagents); per-response
    # usage is the fallback.
    if s["model_usage"]:
        return {
            k: sum(u.get(f, 0) or 0 for u in s["model_usage"].values())
            for k, _, f in TOKEN_FIELDS
        }
    return {
        k: sum(u.get(f, 0) or 0 for _, u in s["usage"].values())
        for k, f, _ in TOKEN_FIELDS
    }


def project(cwd):
    if re.search(r"/Stuff/\d{4}-\d\d/\d\d-[^/]+$", cwd):
        return "Stuff (day dir)"
    return Path(cwd).name


# --- git ---------------------------------------------------------------------


def git(repo, *args):
    try:
        return subprocess.run(
            [GIT, "-C", repo, *args],
            capture_output=True,
            text=True,
            timeout=10,
        ).stdout.strip()
    except (OSError, subprocess.TimeoutExpired):
        return ""


@functools.cache
def toplevel(d):
    return git(d, "rev-parse", "--show-toplevel") or d


def resolve_commits(s):
    """Hashes as on a branch now: -q commits print none, and a rebase during
    push rewrites them."""
    s["commits"], s["unresolved"] = [], 0
    seen = set()
    since = f"--since={(s['start'] - IDLE).isoformat()}"
    for h, subject, repo in s.pop("raw_commits"):
        top = toplevel(repo)
        current = (
            h if h and git(top, "branch", "-a", "--contains", h) else None
        )
        if current is None:
            for line in git(
                top, "log", "--branches", "--remotes", since, "--format=%h %s"
            ).splitlines():
                if line.split(" ", 1)[1:] == [subject]:
                    current = line.split(" ", 1)[0][:7]
                    break
        if current is None and h is None:
            # The commit failed (hook, nothing staged) or the repo is gone.
            s["unresolved"] += 1
            continue
        current = current or h
        if (top, current) in seen:
            # A retry after a failed attempt (signing, hook) resolves to the
            # same commit by subject.
            continue
        seen.add((top, current))
        s["commits"].append(
            dict(
                hash=current,
                subject=subject,
                repo=top,
                was=h if h and h != current else None,
                pushed=pushed(top, current),
            )
        )


def pushed(repo, h):
    """False only if a local branch has the commit and no remote one does: a
    commit amended or rebased away is superseded, not unpushed. Repos
    without a remote never count.
    """
    return (
        not git(repo, "remote")
        or not git(repo, "branch", "--contains", h)
        or bool(git(repo, "branch", "-r", "--contains", h))
    )


# --- cost --------------------------------------------------------------------


def estimate_costs(sessions):
    """Price sessions without a cost record, with per-model rates fitted to
    the sessions that have one."""
    rows = collections.defaultdict(lambda: ([], []))
    for s in sessions:
        for model, u in (s["model_usage"] or {}).items():
            if u.get("costUSD"):
                rows[model][0].append(
                    [u.get(f, 0) or 0 for _, _, f in TOKEN_FIELDS]
                )
                rows[model][1].append(u["costUSD"])
    rates = {
        m: np.linalg.lstsq(np.array(x, float), np.array(y), rcond=None)[0]
        for m, (x, y) in rows.items()
        if len(y) >= 8
    }

    def raw(s):
        per = collections.defaultdict(lambda: np.zeros(4))
        for model, u in s["usage"].values():
            per[model] += [u.get(f, 0) or 0 for _, f, _ in TOKEN_FIELDS]
        if per and all(m in rates for m in per):
            return float(sum(rates[m] @ v for m, v in per.items()))
        return None

    # Per-response usage misses side calls (titles, advisor), so it prices
    # low; the median ratio against known costs corrects that (measured
    # 2026-11-24: 0.74, p10-p90 0.67-0.83).
    ratios = [
        r / s["cost"]
        for s in sessions
        if s["cost"] and not s["agents"] and (r := raw(s))
    ]
    scale = 1 / float(np.median(ratios)) if ratios else 1
    for s in sessions:
        if s["cost"] is None and (r := raw(s)) is not None:
            s["est"] = r * scale


def cost_of(s):
    return s["cost"] if s["cost"] is not None else s.get("est") or 0


# --- render ------------------------------------------------------------------


def cut(s, n):
    return s if len(s) <= n else s[: n - 1] + "…"


def md(s):
    return s.replace("|", "\\|").replace("`", "'")


def home(p):
    return p.replace(str(Path.home()), "~", 1)


def fmt_min(m):
    return "<1m" if m < 1 else f"{m:.0f}m" if m < 90 else f"{m / 60:.1f}h"


def fmt_tok(n):
    return (
        f"{n / 1e6:.1f}M"
        if n >= 1e6
        else f"{n / 1e3:.0f}k" if n >= 1e3 else str(n)
    )


def fmt_tokens(t):
    n_in = t["in"] + t["cache_read"] + t["cache_write"]
    if not n_in:
        return "no tokens"
    cached = t["cache_read"] / n_in
    out = fmt_tok(t["out"])
    return f"in {fmt_tok(n_in)} ({cached:.0%} cached) · out {out}"


def fmt_cost(s):
    if s["cost"] is not None:
        return f"${s['cost']:.2f}"
    if s.get("est") is not None:
        return f"~${s['est']:.2f}" + (" + subagents" if s["agents"] else "")
    return "$?"


def sentences(text):
    text = re.sub(r"```.*?```", " ", text, flags=re.S)
    text = re.sub(r"^\s*(#+|[-*]|\d+\.|\|.*)\s*", "", text, flags=re.M)
    return [
        x.strip()
        for x in re.findall(
            r".+?[.!?](?=\s|$)|.+$", oneline(re.sub(r"[*_`]", "", text))
        )
    ]


def closing(replies):
    """First statement of the last replies, skipping filler: 'Done.',
    'Gern geschehen.', 'I've now read…'."""
    for r in reversed(replies[-3:]):
        for x in sentences(r)[:4]:
            if len(x.split()) >= 8 and not FILLER.match(x):
                return x
    return ""


def file_names(paths):
    """Basenames, with the parent dir where two files share one."""
    dup = {
        n
        for n, c in collections.Counter(Path(f).name for f in paths).items()
        if c > 1
    }
    return [
        (
            f"{Path(f).parent.name}/{Path(f).name}"
            if Path(f).name in dup
            else Path(f).name
        )
        for f in paths
    ]


def trivial(s):
    return (
        len(s["prompts"]) <= 1
        and not s["files"]
        and not s["commits"]
        and not s["artifacts"]
    )


def artifact_line(a, w):
    fallback = "Claude Doc" if a["kind"] == "doc" else "Artifact"
    title = md(cut(a["title"] or fallback, w // 2))
    if a["url"]:
        link = title.replace("[", "\\[").replace("]", "\\]")
        name = f"[{link}]({a['url']})"
    else:
        name = f"{title} `{a['id'][:8]}`"
    page = a["kind"] == "page"
    how = ["new" if a["new"] else "updated" if page else "edited"]
    if a["v"]:
        how.append(f"v{a['v']}" if page else f"rev {a['v']}")
    return f"- ◈ {name} {' · '.join(how)}"


def state(s):
    tags = ["open"] if s["last_kind"] == "user" else []
    return tags + (
        ["unpushed"] if not all(c["pushed"] for c in s["commits"]) else []
    )


def card(s, w=140):
    tags = "⏱ " if s["headless"] else ""
    tags += "".join(f"**[{x}]** " for x in state(s))
    title = s["title"] or cut(s["prompts"][0], 70)
    meta = [
        f"`{s['id']}`",
        f"`{md(home(s['cwd'] or '?'))}`",
        f"{fmt_min(s['active'])} active",
        fmt_cost(s),
        fmt_tokens(s["tokens"]),
        f"{len(s['prompts'])} prompts",
    ]
    if s["agents"]:
        meta.append(f"{s['agents']} subagents")
    if s["artifacts"]:
        meta.append(f"{len(s['artifacts'])} artifacts")
    if s["branch"] and s["branch"] != "HEAD":
        meta.append(f"`{md(s['branch'])}`")
    out = [
        f"### {s['start']:%H:%M} · {md(s['project'])} · {tags}{md(title)}",
        "",
        " · ".join(meta),
        "",
        f"- ▸ {md(cut(s['prompts'][0], w))}",
    ]
    steer = [p for p in s["prompts"][1:] if not ACK.match(p)]
    # First and last steering prompt show where it went; the middle
    # collapses to a count.
    if len(steer) > 2:
        steer = [steer[0], f"… {len(steer) - 2} more …", steer[-1]]
    out += [f"  - ↳ {md(cut(p, w - 4))}" for p in steer]
    for c in s["commits"][-3:]:
        line = f"- ⎇ `{c['hash']}` {md(cut(c['subject'], w - 11))}"
        if c["was"]:
            line += f" (was `{c['was']}`)"
        if not c["pushed"]:
            line += " **[unpushed]**"
        out.append(line)
    if len(s["commits"]) > 3:
        out[-1] += f" (+{len(s['commits']) - 3} earlier)"
    if s["unresolved"]:
        out.append(f"- ⎇ {s['unresolved']} commit attempt(s) not found in git")
    arts = list(s["artifacts"].values())
    out += [artifact_line(a, w) for a in arts[:3]]
    if len(arts) > 3:
        out[-1] += f" (+{len(arts) - 3} more)"
    if s["files"]:
        names = file_names(s["files"])
        more = f" +{len(names) - 8}" if len(names) > 8 else ""
        out.append("- ✎ " + md(", ".join(names[:8]) + more))
    if text := closing(s["replies"]):
        out.append(f"- ◂ {md(cut(text, w))}")
    return "\n".join(out) + "\n"


def quick(ss):
    def one(s):
        name = md(cut(s["title"] or s["prompts"][0], 40))
        if s["project"] != "Stuff (day dir)":
            name += f" ({md(s['project'])})"
        tag = "⏱ " if s["headless"] else ""
        return f"{s['start']:%H:%M} {tag}{name} `{s['id'][:8]}` {fmt_cost(s)}"

    items = " · ".join(one(s) for s in ss)
    return f"**Quick** (1 prompt, nothing made): {items}\n"


LEGEND = """<details><summary>How to read this</summary>

- `### HH:MM · project · title`: one Claude Code session, by start time; it
  stays on its start day even when resumed later. ⏱ = started by a script
  (headless), not typed. **[open]** = ended on a prompt with no reply
  (interrupted or still running). **[unpushed]** = one of its commits is on
  a local branch but no remote one.
- The line under it: Claude Code session ID · working dir · active time (gaps
  over 5 min left out) · cost (recorded costs include subagents; `~` =
  estimated from tokens, `+ subagents` = their share missing) · input tokens
  and the share read from prompt cache · output tokens · prompts typed ·
  subagents · artifacts · git branch.
- ▸ first prompt · ↳ first and last later prompt (acks like "next" left out;
  `[paste]` = pasted text, `[marginal annotations]` = review notes sent from
  Emacs) · ⎇ commit · ◈ artifact page published or Claude Doc created or
  edited (new = first version made in this session; `vN` / `rev N` = its
  version after the session's last change; untitled `Claude Doc` = a doc
  from an earlier session) · ✎ files edited · ◂ first statement of the last
  replies (8+ words, no filler like "Done."), left out when there is none.
- ⎇ `hash` is the commit as on a branch today, `(was …)` when a rebase or
  amend changed it; `(+N earlier)` = more commits not shown; "commit
  attempt(s) not found in git" = `git commit` ran but git has no such
  commit. Git markers reflect git when this file was generated, so they can
  contradict what a reply said then.
- `project` = the working dir (`Stuff (day dir)`: a dated
  `~/Stuff/YYYY-MM/DD-host` dir); commits often land in another repo, so
  **Commits by repo** counts them where they landed. **Quick** = 1 prompt,
  nothing edited, committed or published.
- **Recurring** = same title (ignoring commit hashes) 2+ times, not counting
  hand-typed Quick sessions. **Tools** = tool calls in the main
  conversations, by Claude Code tool name.
- Dig deeper: the transcript is `~/.claude/projects/*/<id>.jsonl`;
  `cd <dir> && claude -r <id>` resumes the session, `<dir>` being the path
  after the ID.

</details>
"""


def total(ss, key):
    """Sum over sessions: 'active', 'cost', or the length of a list field."""
    if key == "cost":
        return sum(cost_of(s) for s in ss)
    if key == "active":
        return sum(s["active"] for s in ss)
    return sum(len(s[key]) for s in ss)


def by_cost(group):
    """Sort key for (name, sessions): costliest first, then by name."""
    return -total(group[1], "cost"), group[0]


def by_count(counter):
    return sorted(counter.items(), key=lambda kv: (-kv[1], kv[0]))


def recurring_key(s):
    """Title, else the prompt's opening; commit hashes in it don't count."""
    key = s["title"] or s["prompts"][0]
    if len(key) > 60:
        key = re.sub(r"\s+\S*$", "…", key[:60])
    return re.sub(r"\s*\b(?=[0-9a-f]*\d)[0-9a-f]{7,40}\b", "", key).strip()


def rollups(sessions):
    o = [
        "## By project",
        "",
        "| project | sessions | active | cost | commits |",
        "|---|--:|--:|--:|--:|",
    ]
    by = collections.defaultdict(list)
    for s in sessions:
        by[s["project"]].append(s)
    for p, ss in sorted(by.items(), key=by_cost):
        active = fmt_min(total(ss, "active"))
        cost, commits = total(ss, "cost"), total(ss, "commits")
        row = f"| {md(p)} | {len(ss)} | {active} | ${cost:.2f} | {commits} |"
        o.append(row)
    repos = collections.Counter(
        c["repo"] for s in sessions for c in s["commits"]
    )
    if repos:
        landed = " · ".join(f"{md(home(r))} {n}" for r, n in by_count(repos))
        o += ["", f"**Commits by repo** (where they landed): {landed}"]
    o += ["", "## Costliest", ""]
    priced = sorted(
        (s for s in sessions if cost_of(s)), key=lambda s: -cost_of(s)
    )
    for s in priced[:5]:
        per = cost_of(s) / max(len(s["prompts"]), 1)
        title = md(s["title"] or cut(s["prompts"][0], 60))
        o.append(
            f"- {fmt_cost(s)} (${per:.2f}/prompt) · {s['start']:%m-%d %H:%M}"
            f" · {title} · `{s['id'][:8]}`"
        )
    tools = sum((s["tools"] for s in sessions), collections.Counter())
    used = " · ".join(f"{n} {c}" for n, c in by_count(tools)[:10])
    o += ["", f"**Tools:** {used}", ""]
    runs = collections.defaultdict(list)
    for s in sessions:
        if not trivial(s) or s["headless"]:  # 'hi' twice is a test
            runs[recurring_key(s)].append(s)
    recurring = sorted(
        ((t, ss) for t, ss in runs.items() if len(ss) >= 2),
        key=by_cost,
    )
    if recurring:
        o += ["## Recurring", "", "| title | runs | active | cost |"]
        o.append("|---|--:|--:|--:|")
        for t, ss in recurring:
            active, cost = fmt_min(total(ss, "active")), total(ss, "cost")
            o.append(f"| {md(t)} | {len(ss)} | {active} | ${cost:.2f} |")
        o.append("")
    return o


def summary(sessions):
    unknown = [s for s in sessions if s["cost"] is None]
    est = sum(s.get("est") is not None for s in unknown)
    unpriced = len(unknown) - est
    notes = [f"{est} sessions estimated"] if est else []
    notes += [f"{unpriced} unpriced"] if unpriced else []
    tok = collections.Counter()
    for s in sessions:
        tok.update(s["tokens"])
    cost = f"**${total(sessions, 'cost'):.2f}**"
    if notes:
        cost += f" ({', '.join(notes)})"
    return " · ".join([
        f"**{len(sessions)} sessions**",
        f"**{fmt_min(total(sessions, 'active'))} active**",
        cost,
        fmt_tokens(tok),
        f"{total(sessions, 'prompts')} prompts",
        f"{total(sessions, 'commits')} commits",
        f"{total(sessions, 'artifacts')} artifacts",
        f"{total(sessions, 'files')} files edited",
    ])


def render(sessions, heading, by_day):
    """by_day: a section per day (several days), or none (one day's file)."""
    o = [f"# {heading}", "", summary(sessions), "", LEGEND]
    if len(sessions) >= ROLLUP_MIN:
        o += rollups(sessions)
    days = collections.defaultdict(list)
    for s in sessions:
        days[s["start"].date() if by_day else None].append(s)
    for d in sorted(days, key=lambda d: d or date.min, reverse=True):
        ss = sorted(days[d], key=lambda s: (s["start"], s["id"]), reverse=True)
        if d:
            o += [f"## {d:%Y-%m-%d %a}", ""]
        o += [card(s) for s in ss if not trivial(s)]
        if q := [s for s in ss if trivial(s)]:
            o.append(quick(q))
    return "\n".join(o)


# --- main --------------------------------------------------------------------


def load(projects, since):
    """Sessions in transcripts modified since `since` (one started then was
    written since)."""
    out = []
    for p in sorted(projects.glob("*/*.jsonl")):
        if datetime.fromtimestamp(p.stat().st_mtime).astimezone() < since:
            continue
        if s := parse(p):
            s["mtime"] = datetime.fromtimestamp(p.stat().st_mtime).astimezone()
            resolve_commits(s)
            out.append(s)
    estimate_costs(out)
    return out


def midnight(d):
    return datetime.combine(d, time()).astimezone()


def days_to_write(sessions, today):
    """Today, yesterday, and the start day of every session written to since
    yesterday."""
    yesterday = today - timedelta(days=1)
    return {today, yesterday} | {
        s["start"].date()
        for s in sessions
        if s["mtime"] >= midnight(yesterday)
    }


def write_day(stuff, host, d, sessions):
    """'written', 'unchanged' or 'no dir': day dirs belong to stuff-today and
    are never created here."""
    day_dir = stuff / f"{d:%Y-%m}" / f"{d:%d}-{host}"
    if not day_dir.is_dir():
        return "no dir"
    target = day_dir / FILE_NAME
    text = render(
        sessions,
        f"Claude Code sessions {d:%Y-%m-%d %a} ({host})",
        by_day=False,
    )
    if target.exists() and target.read_text() == text:
        return "unchanged"
    tmp = target.with_name(f".{FILE_NAME}.tmp")
    tmp.write_text(text)
    tmp.replace(target)
    return "written"


def main(argv=None):
    ap = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    ap.add_argument("--write", action="store_true")
    ap.add_argument(
        "--host",
        help="host tag in DD-<host> (config.my.hostTag); needed by --write",
    )
    group = ap.add_mutually_exclusive_group()
    group.add_argument(
        "--date",
        type=date.fromisoformat,
        help="print this day (default: today)",
    )
    group.add_argument(
        "--days", type=int, help="print the last N days, one section per day"
    )
    ap.add_argument(
        "--projects",
        type=Path,
        default=Path.home() / ".claude/projects",
        help=argparse.SUPPRESS,
    )
    ap.add_argument(
        "--stuff",
        type=Path,
        default=Path.home() / "Stuff",
        help=argparse.SUPPRESS,
    )
    a = ap.parse_args(argv)
    today = date.today()
    if a.write:
        if not a.host:
            ap.error("--write needs --host")
        sessions = load(a.projects, midnight(today - timedelta(days=1)))
        days = days_to_write(sessions, today)
        sessions = (
            load(a.projects, midnight(min(days)))
            if min(days) < today - timedelta(days=1)
            else sessions
        )
        for d in sorted(days):
            mine = [s for s in sessions if s["start"].date() == d]
            print(
                f"{d}: {write_day(a.stuff, a.host, d, mine)}"
                f" ({len(mine)} sessions)",
                file=sys.stderr,
            )
        return
    first = today - timedelta(days=a.days - 1) if a.days else a.date or today
    last = today if a.days else first
    sessions = [
        s
        for s in load(a.projects, midnight(first))
        if first <= s["start"].date() <= last
    ]
    heading = f"Claude Code sessions {first:%Y-%m-%d}" + (
        f" – {last:%Y-%m-%d}" if last != first else f" {first:%a}"
    )
    print(render(sessions, heading, by_day=bool(a.days)))


if __name__ == "__main__":
    os.environ.setdefault("GIT_TERMINAL_PROMPT", "0")
    main()
