"""Tests for session-digest.py: synthetic transcripts and throwaway git
repos, no real data.

Run: SESSION_DIGEST=path/to/session-digest.py pytest session-digest-test.py
"""

import importlib.util
import json
import os
import subprocess
from datetime import date, datetime, timedelta, timezone
from pathlib import Path

import pytest

spec = importlib.util.spec_from_file_location(
    "sd",
    os.environ.get(
        "SESSION_DIGEST", Path(__file__).with_name("session-digest.py")
    ),
)
sd = importlib.util.module_from_spec(spec)
spec.loader.exec_module(sd)

T0 = datetime(2026, 11, 20, 9, 0, tzinfo=timezone.utc)


def ts(minutes):
    return (T0 + timedelta(minutes=minutes)).isoformat().replace("+00:00", "Z")


class Transcript:
    """A transcript the way Claude Code writes one, a record per line."""

    def __init__(self, cwd="/work", entrypoint="cli"):
        self.rows, self.cwd, self.entrypoint, self.n = [], cwd, entrypoint, 0

    def _base(self, kind, minute, **kw):
        return dict(
            type=kind,
            timestamp=ts(minute),
            cwd=self.cwd,
            entrypoint=self.entrypoint,
            gitBranch="main",
            isSidechain=False,
            **kw,
        )

    def user(self, minute, text, **kw):
        self.rows.append(
            self._base(
                "user", minute, message=dict(role="user", content=text), **kw
            )
        )
        return self

    def reply(self, minute, text, usage=None, model="m1"):
        self.n += 1
        self.rows.append(
            self._base(
                "assistant",
                minute,
                message=dict(
                    id=f"msg{self.n}",
                    model=model,
                    content=[dict(type="text", text=text)],
                    usage=usage or dict(input_tokens=10, output_tokens=5),
                ),
            )
        )
        return self

    def bash(self, minute, command, output):
        self.n += 1
        tid = f"tool{self.n}"
        self.rows.append(
            self._base(
                "assistant",
                minute,
                message=dict(
                    id=f"msg{self.n}",
                    model="m1",
                    content=[
                        dict(
                            type="tool_use",
                            id=tid,
                            name="Bash",
                            input=dict(command=command),
                        )
                    ],
                ),
            )
        )
        self.rows.append(
            self._base(
                "user",
                minute,
                message=dict(
                    role="user",
                    content=[
                        dict(
                            type="tool_result", tool_use_id=tid, content=output
                        )
                    ],
                ),
            )
        )
        return self

    def edit(self, minute, path):
        self.n += 1
        self.rows.append(
            self._base(
                "assistant",
                minute,
                message=dict(
                    id=f"msg{self.n}",
                    model="m1",
                    content=[
                        dict(
                            type="tool_use",
                            id=f"e{self.n}",
                            name="Edit",
                            input=dict(file_path=path),
                        )
                    ],
                ),
            )
        )
        return self

    def raw(self, **record):
        self.rows.append(record)
        return self

    def save(self, projects, sid="11111111-aaaa-bbbb-cccc-000000000000"):
        path = projects / "-work" / f"{sid}.jsonl"
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("\n".join(json.dumps(r) for r in self.rows) + "\n")
        return path


def git(repo, *args):
    return subprocess.run(
        ["git", "-C", str(repo), *args],
        check=True,
        capture_output=True,
        text=True,
        env={
            **os.environ,
            "GIT_AUTHOR_NAME": "t",
            "GIT_AUTHOR_EMAIL": "t@t",
            "GIT_COMMITTER_NAME": "t",
            "GIT_COMMITTER_EMAIL": "t@t",
            "GIT_AUTHOR_DATE": T0.isoformat(),
            "GIT_COMMITTER_DATE": T0.isoformat(),
        },
    ).stdout.strip()


@pytest.fixture
def repo(tmp_path):
    """A repo with a pushed first commit and a bare remote."""
    remote, work = tmp_path / "remote.git", tmp_path / "work"
    subprocess.run(
        ["git", "init", "-q", "--bare", "-b", "main", str(remote)], check=True
    )
    subprocess.run(["git", "init", "-q", "-b", "main", str(work)], check=True)
    (work / "a").write_text("a")
    git(work, "add", "a")
    git(work, "commit", "-qm", "first")
    git(work, "remote", "add", "origin", str(remote))
    git(work, "push", "-q", "origin", "main")
    return work


def parsed(tmp_path, t):
    s = sd.parse(t.save(tmp_path / "projects"))
    sd.resolve_commits(s)
    return s


def test_prompts_skip_setup_acks_and_injected_context(tmp_path):
    t = (
        Transcript()
        .user(
            0,
            "<command-name>/model</command-name>"
            "<command-args>fable</command-args>",
        )
        .user(
            1,
            "<command-name>/review</command-name>\n"
            "<command-args>abc</command-args>",
        )
        .user(2, "<system-reminder>injected</system-reminder>")
        .user(
            3,
            'fix the <pasted_content id="x">secret stuff</pasted_content> bug',
        )
        .user(4, "next")
        .user(5, MARGINAL_PROMPT)
    )
    s = parsed(tmp_path, t)
    assert s["prompts"] == [
        "/review abc",
        "fix the [paste] bug",
        "next",
        "[marginal annotations]",
    ]
    out = sd.card(s)
    assert "▸ /review abc" in out and "↳ fix the [paste] bug" in out
    assert "↳ next" not in out


MARGINAL_PROMPT = (
    "My annotations on your last message, from marginal. Under each heading…"
)


def test_usage_counted_once_per_response_and_open_state(tmp_path):
    usage = dict(
        input_tokens=100, output_tokens=50, cache_read_input_tokens=900
    )
    t = (
        Transcript()
        .user(0, "question")
        .reply(1, "part one", usage)
        .reply(1, "part two", usage)
    )
    t.rows[-1]["message"]["id"] = t.rows[-2]["message"][
        "id"
    ]  # one response, two records
    t.user(2, "and then?")
    s = parsed(tmp_path, t)
    assert s["tokens"] == dict(
        **{"in": 100, "out": 50, "cache_read": 900, "cache_write": 0}
    )
    assert sd.state(s) == ["open"]
    assert "in 1k (90% cached) · out 50" in sd.card(s)


def test_cost_state_wins_over_usage(tmp_path):
    t = (
        Transcript()
        .user(0, "q")
        .reply(1, "a")
        .raw(
            type="cost-state",
            totalCostUSD=1.5,
            modelUsage={
                "m1": dict(
                    inputTokens=1,
                    outputTokens=2,
                    cacheReadInputTokens=3,
                    cacheCreationInputTokens=4,
                    costUSD=1.5,
                )
            },
        )
    )
    s = parsed(tmp_path, t)
    assert s["cost"] == 1.5 and s["tokens"]["cache_write"] == 4
    assert "$1.50" in sd.card(s)


def test_headless_only_from_entrypoint(tmp_path):
    typed = Transcript().user(
        0, "hi", promptSource="sdk"
    )  # a message sent into an interactive session
    assert not parsed(tmp_path, typed)["headless"]
    assert parsed(tmp_path, Transcript(entrypoint="sdk-cli").user(0, "hi"))[
        "headless"
    ]


def test_quiet_commit_resolved_from_log_line_and_unpushed(tmp_path, repo):
    (repo / "b").write_text("b")
    git(repo, "add", "b")
    git(repo, "commit", "-qm", "add b")
    h = git(repo, "rev-parse", "--short=7", "HEAD")
    t = (
        Transcript(cwd=str(repo))
        .user(0, "commit it")
        .bash(
            1, 'git commit -qm "add b" && git log --oneline -1', f"{h} add b"
        )
        .reply(2, "ok")
    )
    s = parsed(tmp_path, t)
    assert [(c["hash"], c["pushed"]) for c in s["commits"]] == [(h, False)]
    assert sd.state(s) == ["unpushed"]
    git(repo, "push", "-q", "origin", "main")
    assert parsed(tmp_path, t)["commits"][0]["pushed"]


def test_silent_commit_found_in_git_by_subject(tmp_path, repo):
    (repo / "c").write_text("c")
    git(repo, "add", "c")
    git(repo, "commit", "-qm", "add c")
    h = git(repo, "rev-parse", "--short=7", "HEAD")
    heredoc = "git commit -q -F - <<'EOF'\nadd c\n\nbody\nEOF"
    t = (
        Transcript(cwd=str(repo))
        .user(0, "go")
        .bash(1, f"cd {repo} && {heredoc}", "")
    )
    assert [c["hash"] for c in parsed(tmp_path, t)["commits"]] == [h]


def test_rebased_commit_shows_current_hash(tmp_path, repo):
    (repo / "d").write_text("d")
    git(repo, "add", "d")
    git(repo, "commit", "-qm", "add d")
    old = git(repo, "rev-parse", "--short=7", "HEAD")
    git(
        repo,
        "commit",
        "-q",
        "--amend",
        "--no-edit",
        "--date",
        (T0 + timedelta(minutes=3)).isoformat(),
    )
    new = git(repo, "rev-parse", "--short=7", "HEAD")
    assert old != new
    git(
        repo, "reflog", "expire", "--expire=now", "--all"
    )  # the old commit is now on no branch
    t = (
        Transcript(cwd=str(repo))
        .user(0, "go")
        .bash(1, 'git commit -m "add d"', f"[main {old}] add d")
    )
    c = parsed(tmp_path, t)["commits"][0]
    assert (c["hash"], c["was"]) == (new, old)


def test_failed_commit_is_counted_not_listed(tmp_path, repo):
    t = (
        Transcript(cwd=str(repo))
        .user(0, "go")
        .bash(1, 'git commit -m "never made"', "nothing to commit")
    )
    s = parsed(tmp_path, t)
    assert s["commits"] == [] and s["unresolved"] == 1


def test_closing_skips_filler(tmp_path):
    t = (
        Transcript()
        .user(0, "q")
        .reply(
            1,
            "The config now builds and the timer runs hourly on every host"
            " with transcripts.",
        )
        .reply(2, "Done.")
    )
    assert sd.closing(parsed(tmp_path, t)["replies"]).startswith(
        "The config now builds"
    )
    assert (
        sd.closing(
            [
                "Gern geschehen.",
                "Let me check that again before we go on with it.",
            ]
        )
        == ""
    )


def test_trivial_sessions_fold_into_quick_line(tmp_path):
    projects = tmp_path / "projects"
    Transcript().user(0, "hi").reply(1, "hello").save(
        projects, "a" * 8 + "-0000-0000-0000-000000000000"
    )
    Transcript().user(0, "do it").edit(1, "/x/f.py").user(2, "more").save(
        projects, "b" * 8 + "-0000-0000-0000-000000000000"
    )
    out = sd.render(
        sd.load(projects, T0 - timedelta(days=365)), "day", by_day=False
    )
    assert (
        "**Quick** (1 prompt, nothing edited): " in out and "`aaaaaaaa`" in out
    )
    assert out.count("\n### ") == 1 and "✎ f.py" in out


def test_write_day_only_on_change_and_never_creates_day_dirs(tmp_path):
    projects, stuff = tmp_path / "projects", tmp_path / "stuff"
    Transcript().user(0, "q").reply(1, "a").save(projects)
    sessions = sd.load(projects, T0 - timedelta(days=365))
    d = sessions[0]["start"].date()
    assert sd.write_day(stuff, "h", d, sessions) == "no dir"
    day_dir = stuff / f"{d:%Y-%m}" / f"{d:%d}-h"
    day_dir.mkdir(parents=True)
    assert sd.write_day(stuff, "h", d, sessions) == "written"
    assert sd.write_day(stuff, "h", d, sessions) == "unchanged"
    assert (
        (day_dir / sd.FILE_NAME)
        .read_text()
        .startswith(f"# Claude Code sessions {d:%Y-%m-%d}")
    )


def test_resumed_session_rewrites_its_start_day():
    today = date(2026, 11, 24)
    old = dict(
        start=datetime(2026, 11, 3, 10, tzinfo=timezone.utc).astimezone(),
        mtime=sd.midnight(today),
    )
    stale = dict(
        start=datetime(2026, 11, 2, 10, tzinfo=timezone.utc).astimezone(),
        mtime=sd.midnight(today - timedelta(days=5)),
    )
    assert sd.days_to_write([old, stale], today) == {
        today,
        today - timedelta(days=1),
        old["start"].date(),
    }
