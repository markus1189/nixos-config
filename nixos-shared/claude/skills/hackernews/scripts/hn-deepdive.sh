#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#coreutils --command bash
# shellcheck shell=bash
set -euo pipefail

# Deep-dive into a single HN story using a sub-agent with isolated context.
# Spawns `pi -p --no-session` to fetch article + comments, returns structured summary.
#
# Usage: hn-deepdive.sh <story_id> <article_url> <story_title>
#        hn-deepdive.sh <story_id> "" <story_title>    # no article URL (Ask HN, etc.)
#
# Output: Structured markdown summary on stdout. Progress on stderr.

readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [[ $# -lt 3 ]]; then
    echo "Usage: ${0##*/} <story_id> <article_url> <story_title>" >&2
    exit 1
fi

readonly STORY_ID="$1"
readonly ARTICLE_URL="$2"
readonly STORY_TITLE="$3"

# Build the prompt for the sub-agent
build_prompt() {
    local fetch_article_instruction=""
    if [[ -n "$ARTICLE_URL" ]]; then
        fetch_article_instruction="
1. Fetch the article content:
   curl -sL '$ARTICLE_URL' | pandoc -f html -t gfm-raw_html
   If this fails or returns garbage, note it and move on."
    fi

    cat <<PROMPT
You are summarizing a Hacker News story for a technical user. Be direct, opinionated, and technically accurate.

## Tasks
${fetch_article_instruction}
$([[ -n "$fetch_article_instruction" ]] && echo "2." || echo "1.") Fetch the HN comments:
   ${SCRIPT_DIR}/hn-cli.sh -c ${STORY_ID} -d 2 -n 50

## Output Format

Return a single structured markdown summary with these sections:

### ${STORY_TITLE}

**TL;DR**: 2-3 sentence summary of the article content (or the Ask HN question).

**Key Points**:
- Bullet points of the most important facts/claims from the article

**Key Quotes** (if notable):
> Direct quotes from the article that matter

**HN Discussion Themes**:
| Theme | Sentiment | Key Arguments |
|-------|-----------|---------------|
| ...   | ...       | ...           |

**Notable Comments**:
> Quote the most insightful or contrarian comments with attribution

**Linked Artifacts** (if any — be selective):
Only surface links that are personal, hand-crafted, or validated by the thread (praised, discussed, built upon). Skip generic libraries, well-known projects, and obvious self-promotion. Good finds: someone's dotfiles, personal tool repos, AGENTS.md/SKILL.md files, gists with workflows, niche tools they built themselves. Include: commenter name, what it is, why it's interesting, and the full URL.

**Meta**: Any drama, astroturfing signals, or interesting discussion dynamics.

## Rules
- Skip sections that don't apply (e.g., no Key Quotes for Ask HN posts)
- Be concise. This summary replaces reading the full article + comments.
- Preserve technical accuracy. Don't simplify jargon.
- If the article is paywalled or empty, say so and focus on comments.
- Output ONLY the markdown summary, nothing else.
PROMPT
}

echo "⏳ Deep-diving into [${STORY_ID}] ${STORY_TITLE}..." >&2

PROMPT="$(build_prompt)"
RESULT="$(echo "$PROMPT" | pi -p --no-session 2>/dev/null)"

echo "$RESULT"
echo "✓ Done: [${STORY_ID}] ${STORY_TITLE}" >&2
