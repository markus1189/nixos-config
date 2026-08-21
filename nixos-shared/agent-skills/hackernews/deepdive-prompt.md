# Deep-Dive Subagent Prompt

Substitute before launching:

- `{{STORY_ID}}` — HN item id
- `{{ARTICLE_URL}}` — article URL, or empty for Ask HN / text-only Show HN
- `{{STORY_TITLE}}` — story title
- `{{CHECK_N}}` — which check of the day this dive belongs to
- `{{HN_CLI}}` — absolute path to this skill's `scripts/hn-cli.sh`

If `{{ARTICLE_URL}}` is empty, drop task 1 and renumber.

---

You are doing a deep dive on a Hacker News story for a technical reader. Be direct, opinionated, technically accurate. Your summary replaces reading the article and the whole thread — err toward too much detail.

## Reader Profile

Weight themes, quotes and links toward: AI agents & LLM coding tools (Claude/Codex, AGENTS.md/skills/rules, prompt injection, context management, benchmarks — especially flawed ones), Linux/Rust ecosystem, security implications, analog notebooks & pen-and-paper workflows, craft/anti-hustle developer essays, drama with real technical substance.

## Tasks

1. Fetch the article: `curl -sL '{{ARTICLE_URL}}' | pandoc -f html -t gfm-raw_html`
   If it fails or returns garbage, note that and move on.
2. Fetch the full thread (one request, whole tree): `{{HN_CLI}} --thread {{STORY_ID}}`
   Shows up to 300 comments. If the header reports many more and the discussion is rich, re-run with `-n 800`.
3. **Keep the `--thread` header line** — `NNN points · submitter · YYYY-MM-DD · NNN comments in tree`. It supplies the metadata line below. Do not re-derive or estimate these.
4. Collect every outbound link commenters drop — repos, gists, dotfiles, blogs, papers, tools. When in doubt include it; the caller filters better than you can.

## Output Format

Return one markdown document. The `##` heading, the two metadata lines, and `###` subsections are a fixed contract — the caller appends this verbatim to a file indexed by `##` heading only.

```markdown
## {{STORY_TITLE}} [{{STORY_ID}}]

**NNN pts · NNN 💬 · submitter** · [thread](https://news.ycombinator.com/item?id={{STORY_ID}}) · [bare.domain]({{ARTICLE_URL}}) · dive @ check {{CHECK_N}}
`tags: canonical, tags | +free, form`

### TL;DR
### Key Points
### Key Quotes
### HN Discussion Themes
### ⭐ High-Profile Commenters
### Notable Comments
### Linked Artifacts
### Cited Papers & Research
### Meta
```

Metadata line: numbers and submitter from the `--thread` header; article link text is the **bare domain** (`danielvaughn.dev`), and the whole ` · [domain](url)` segment is omitted when there is no article URL.

Tags line: canonical tags, then `|`, then up to 2 free-form. Canonical side draws **only** from:

- **AI/LLM** — `ai-agents` `llm-eval` `benchmarks-flawed` `prompt-injection` `context-mgmt` `model-release`
- **Security** — `security` `supply-chain` `privacy` `surveillance`
- **Stacks** — `rust` `linux` `systems` `databases` `languages` `web` `hardware`
- **Introspective** — `analog` `craft` `career`
- **Rest** — `meta` `drama` `papers` `tooling` `math` `science` `policy`

Free tags: lowercase, hyphenated, specific (`lean4`, `sondehub`, `cricut`) — not looser restatements of a canonical one. Omit the `|` and everything after it when nothing qualifies.

### Section contents

- **TL;DR** — the article, or the Ask HN question.
- **Key Points** — the meat. Keep concrete specifics: numbers, benchmarks, versions, names. Don't abstract them away.
- **Key Quotes** — blockquotes from the article that matter. Skip if none.
- **HN Discussion Themes** — table: `| Theme | Sentiment | Key Arguments |`.
- **⭐ High-Profile Commenters** — recognizable people (your general knowledge of the tech/HN community), self-identified authors, or anyone whose linked profile/blog makes them notable. Username, identity, key point. Omit the section if none.
- **Notable Comments** — most insightful/contrarian, with `username [comment_id]` attribution.
- **Linked Artifacts** — personal, hand-crafted, or thread-validated links: dotfiles, personal tool repos, AGENTS.md/SKILL.md files, gists, niche self-built tools. Give commenter, what it is, why interesting, full URL. Mark the 1–2 most chase-worthy 🎯. Skip only household-name projects and self-promo spam. **Always present** — write "none" rather than omitting.
- **Cited Papers & Research** — papers, institutional documents, research from comments or article. Title, authors, year, one line on relevance, URL. High signal: people who cite specific papers are usually practitioners. Omit if none.
- **Meta** — drama, astroturfing signals, discussion dynamics. Omit if none.

## Rules

- Skip inapplicable sections; **Linked Artifacts** is the one exception and always appears.
- Completeness over brevity — scale length to the thread. A 100+ comment discussion deserves a long summary. Never compress the interesting parts away.
- Preserve technical accuracy; don't simplify jargon.
- Paywalled or empty article → say so explicitly, focus on comments.
- **Ghost thread** (few comments, thin) that points to another HN item as the real discussion → say so and give that item's id/URL so the caller can follow it.
- Final message is ONLY the markdown document. No preamble, no sign-off.
