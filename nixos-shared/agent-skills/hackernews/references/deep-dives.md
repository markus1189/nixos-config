# Hacker News — deep dives

## Deep-Dive Sub-Agent

**Prompt template**: [`deepdive-prompt.md`](../deepdive-prompt.md)

A deep dive runs as a **subagent** — it fetches the article + comments and returns a
structured markdown summary, keeping the raw content out of the main context.

**How to launch one:**
1. Read [`deepdive-prompt.md`](../deepdive-prompt.md).
2. Substitute `{{STORY_ID}}`, `{{ARTICLE_URL}}` (empty for Ask HN / no link),
   `{{STORY_TITLE}}`, `{{CHECK_N}}` (which check this dive belongs to), `{{HN_CLI}}` →
   absolute path to `scripts/hn-cli.sh`, resolving `./scripts/` against the directory
   containing SKILL.md, `{{READER_PROFILE}}` → the body of [interests.md](interests.md) (everything below its title),
   and `{{TAG_VOCAB}}` → the bullet list under [Tag vocabulary](#tag-vocabulary). Drop the
   article-fetch step when there's no URL.
3. Spawn a subagent with that filled-in text as its prompt. Its result *is* the summary —
   read it directly, no temp files.

**Re-dive** (story already dived today): don't repeat the dive. Have the subagent read the
earlier dive (`treemd -s "<Title> [ID]" hn-daily.md`) and dump only
`-t ID --since "<date> <HH:MM of the Check heading that dive sits under>"`, then report what is
new, reinforced or contradicted. Append it as `## <Title>: thread delta [ID]`.

**Parallel deep-dives** — spawn **multiple deep-dive subagents in parallel**; collect each
one's summary when they return. (No `&`/`wait`, no `/tmp` files.)

Output is a `##` heading + metadata line + `###` subsections (TL;DR, Key Points, HN Discussion
Themes, ⭐ High-Profile Commenters, Notable Comments, Linked Artifacts, Cited Papers, Meta) —
append it verbatim, see "Dive metadata contract" below.

### Dive metadata contract

`##` heading, two metadata lines, `###` subsections. Same in **both** `hn-daily.md` and
`hn-wrapup.md`.

```markdown
## Show HN: Huzzah – a novel approach to coding with AI [49378768]

**350 pts · 198 💬 · danielvaughn** · [thread](https://news.ycombinator.com/item?id=49378768) · [danielvaughn.dev](https://www.danielvaughn.dev/posts/huzzah/) · dive @ check 1
`tags: ai-agents, tooling | +pseudocode, source-maps`

### TL;DR
…
```

Every field is in the `hn-cli.sh --thread` header (`350 points · danielvaughn · 2026-08-20 ·
198 comments in tree`, then the article URL). No extra fetch.

- Article link text = bare domain. Omit that link entirely for Ask HN / text-only Show HN.
- `dive @ check N` in the daily file only; drop it in the wrap-up.
- Tags line backticked and alone, so `rg 'tags:.*prompt-injection'` hits headers, not prose.

### Tag vocabulary

`tags: <canonical, ...> | +<free, ...>` — free side max 2, lowercase, hyphenated, specific
(`lean4`, `sondehub`, `cricut`), not a looser restatement of a canonical tag. Canonical side
draws only from:

- **AI/LLM** — `ai-agents` `llm-eval` `benchmarks-flawed` `prompt-injection` `context-mgmt` `model-release`
- **Security** — `security` `supply-chain` `privacy` `surveillance`
- **Stacks** — `rust` `linux` `systems` `databases` `languages` `web` `hardware`
- **Introspective** — `analog` `craft` `career`
- **Rest** — `meta` `drama` `papers` `tooling` `math` `science` `policy`

Promote a free tag into the canonical list once it recurs in 3 dives.

### Ghost thread auto-follow

If a deep dive returns a **ghost thread** (≤5 comments, thin discussion) that **explicitly links to another HN item** as "the real discussion," automatically launch a second deep-dive subagent on that linked thread — do not ask the user first. Treat it as part of the same request. Present the result under the original story's heading with a note like: *"Ghost thread → auto-followed to [47114579]"*.

## Comment Mining

The most valuable HN finds are often **buried in comments**, not in the stories themselves — someone's personal shell function, a workflow hack, an unpublished tool that lives only in their rc file, etc. When a thread is rich (productivity, "how do you X", Ask HN, "what's your setup"), don't just summarize the article — **scan comments for personal systems, tools, and workflows** people mention (casually).

### What to look for
- **Unpublished personal inventions**: shell functions, directory layouts, automation scripts nobody's packaged. Mentioned across multiple threads = battle-tested.
- **"Show and tell" derails**: When a thread devolves into "what's YOUR setup" — that's the gold, not the article.

### Following up on interesting commenters
Use the Algolia API to check if they've mentioned the same system before:
```bash
curl -s "https://hn.algolia.com/api/v1/search?query=KEYWORDS&tags=comment,author_USERNAME" | jq '.hits[]'
```
Also check: HN profile (`about` field), GitHub username, dotfiles repos, blog links.

### Linked Artifacts — the real gold

The most valuable HN finds are often **linked in comments, not described** — someone drops a GitHub URL to their dotfiles, a gist with their shell function, an AGENTS.md, a SKILL.md, a personal tool repo. These are the discoveries users care about most.

**Filtering**: Surface links that are personal/hand-crafted (someone's own config, dotfiles, tool), validated by the thread (other commenters engaged with it), and relevant to the [user interests](interests.md). Skip generic libraries and well-known projects.

**During deep dives**: The sub-agent surfaces these in a "Linked Artifacts" section (🎯 = most chase-worthy). After every deep-dive batch, aggregate them into a dedicated **🔗 Artifacts & Papers** block in the response — its own section, never buried inside comment quotes. The block always appears; if no dive surfaced anything, say so in one line.

**Chase proactively**: For 1–3 artifacts per batch that match User Interests, don't ask — fetch the README/gist/file immediately (read-only) and attach a 2–3 sentence verdict: what it actually is, whether it's worth adopting. Ask first only when a chase would become a rabbit hole (cloning repos, long papers, multi-repo spelunking).

### Comment Highlights — cross-cutting

After every deep-dive batch, add a **💬 Comment Highlights** block next to 🔗 Artifacts:
5–15 comments picked across all dives, grouped by User Interest (not by story), ranked
by relevance to the user rather than by points. Row: `[user](https://news.ycombinator.com/item?id=ID)` ·
story · one clause on why it matters. ⭐ commenters first; close with the top 5.
Build it from dive output only — no re-fetching.

### Cited Papers & Research

Academic papers, formal institutional documents, and research cited in comments are **always worth surfacing** — treat them like linked artifacts but with higher signal. Commenters who cite specific papers (not just "I read somewhere that...") are usually practitioners with domain expertise. The sub-agent includes a "Cited Papers & Research" section in its output. After reading deep-dive results, call these out prominently alongside linked artifacts.

## Primary Sources for New Releases

For stories about **new model releases** (GPT, Claude, DeepSeek, Gemini, Llama, etc.): after presenting deep-dive summaries, **proactively offer to chase** the system card, technical report, or paper. Find them via the announcement URL, comments, or known locations (e.g., `deploymentsafety.openai.com`, arxiv, HuggingFace model cards). Distill: architecture, full benchmark grid (not cherry-picked), pricing, safety/alignment findings, and practical gotchas.

**When the user asks to chase an artifact**:
1. Fetch the actual content — `curl` the raw GitHub URL, read the file, present it
2. Navigate the repo structure if needed (API: `https://api.github.com/repos/OWNER/REPO/contents/PATH`)
3. Pull the commenter's history via Algolia for prior mentions
4. Search GitHub/web for related work by the same person

## High-Profile Commenters

When a recognizable person shows up in a thread, their comment carries more weight than an anon's — they're speaking from a track record. Flag them explicitly, don't just quote them as "a commenter."

Use your general knowledge to recognize notable HN usernames — you know who they are. Also catch people who identify themselves in-thread ("I'm the author of X", "I work at Y") or whose linked profile/blog makes them notable.

### How to surface them

- In deep dives: add a **⭐ High-Profile Commenters** section before Notable Comments listing who appeared, their identity, and their key point
- In main context briefings: call it out inline if a notable person is the submitter or a prominent commenter (e.g., "Armin Ronacher is in the thread and disagrees with the framing")
- Weight their comments higher — quote them first, even if the comment isn't the most upvoted

