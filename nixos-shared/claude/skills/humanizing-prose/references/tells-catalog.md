# AI Writing Tells Catalog

Detection inventory for reviewing text. Two separate questions per entry: *is this worth fixing as writing* (often yes even for dead tells) and *does this indicate AI involvement* (the tier). Never conflate them, and never present a style review as an authorship determination — every pattern here also occurs in some human writing.

Tell lists decay: vocabulary tells rotate within roughly a model generation (12–18 months), so use them to *date* text, not to adjudicate it. The actively maintained live reference is [Wikipedia:Signs of AI writing](https://en.wikipedia.org/wiki/Wikipedia:Signs_of_AI_writing); consult it when something here feels stale. Catalog last revised: July 2026.

## Contents
- [Tier Legend](#tier-legend)
- [Tier 0: Provenance and Chat Residue](#tier-0-provenance-and-chat-residue)
- [Phrase Tells](#phrase-tells)
- [Vocabulary Tells (era-stratified)](#vocabulary-tells-era-stratified)
- [Register and Tone Tells](#register-and-tone-tells)
- [Structural Tells](#structural-tells)
- [Discourse Tells](#discourse-tells)
- [Typographic Tells](#typographic-tells)
- [Do Not Flag](#do-not-flag)
- [Legacy Indicators (2022-2024)](#legacy-indicators-2022-2024)
- [Model Idiolects](#model-idiolects)
- [Sources and Staleness](#sources-and-staleness)

## Tier Legend

- **Tier 0** — provenance artifacts; near-certain on a single hit. Check these first; one outweighs any amount of style evidence.
- **Tier 1** — strongly diagnostic; flag on one or two occurrences.
- **Tier 2** — cluster signals; flag when they recur or co-occur with other tells, never in isolation.
- **Tier 3** — weak; mention only as supporting evidence, never load-bearing.
- **Style-only** — worth fixing as writing; carries no diagnostic weight anymore.
- **Legacy** — dates text to 2022-2024 model output (vocabulary sub-era: 2023–mid-2024); weak evidence about current text.

## Tier 0: Provenance and Chat Residue

Leaked machinery from the chat interface. When writing or rewriting, the instruction is simply: never emit any of these.

- **Conversational scaffolding**: "Great question!", "You're absolutely right", "Certainly!", "Of course!", "I hope this helps", "Thanks for calling that out" — assistant-to-user speech pasted into prose.
- **Footer offers**: a closing question offering a variant — "Would you like a slightly more formal version?"
- **Unfilled placeholders**: "[Your Name]", "[Entertainer's Name]", "2025-XX-XX", "INSERT_SOURCE_URL", "PASTE_LINK_HERE".
- **Tracking parameters on cited URLs**: `utm_source=chatgpt.com` / `utm_source=openai` / `utm_source=copilot.com`, `referrer=grok.com`. Proves tool involvement in sourcing, though not necessarily in the prose itself.
- **Leaked citation/export tokens**: `turn0search0`, `:contentReference[oaicite:0]`, `oai_citation`, `[cite: 17]`, `grok_card` / `grok_render_citation_card_json`, `【85†L261-269】`, `:::writing{variant="document" id="12345"}` wrappers, stray `↩` footnote arrows. These specific strings are illustrative of an evolving class (as of mid-2026) — treat *any* unexplained machine-looking token or wrapper the same way.
- **Markdown residue in a non-markdown medium**: literal `**asterisks**` or `##` headings on platforms that don't render them, single/mismatched asterisks, skipped heading levels (starting at h3 with no h2).
- **AI-written change descriptions**: edit summaries, commit messages, or PR text in formal first-person paragraphs itemizing what was "ensured," "adhered to," or "avoided," verbosely justifying trivial changes. Strong evidence the change itself is AI-generated.

Strong but *not* conclusive (humans do these too): sections the assignment never asked for; headings pasted in mismatched sizes; title-case headings where the document's convention is sentence case.

## Phrase Tells

**Negative parallelism family** [Tier 1; intensified 2025–2026 — currently the top phrase-level tell]. Base form "It's not X, it's Y" plus variants:
- Staccato negation triad: "Not a bug. Not a feature. A fundamental design flaw." / "No hardware. No fees. Just growth."
- Cross-sentence reframe: "The question isn't X. The question is Y."
- Causal form: "not because X, but because Y"
- Em-dash dismissal: "X — not Y"
- "X rather than Y" — common in ordinary human prose; flag only when it recurs several times in one piece (reported especially frequent in some models, e.g. Grok per Wikipedia editors).

Fix: delete the dismissed half and state the actual claim. The first four variants are per-instance signals; one "rather than" is noise.

**Trailing participle commentary** [Tier 1]: "...highlighting its enduring legacy", "...ensuring a seamless experience", "...contributing to the broader ecosystem". RAG-era twist: newer chatbots attach these canned analyses to *named sources or real-looking citations* ("Roger Ebert highlighted the lasting influence...") regardless of what the source says — verify the source, don't just style-check.

**Significance inflation** [Tier 1]: "...marking a pivotal moment in...", "...setting the stage for...", "...stands as a testament to...", "...underscores its importance...".

**Notability/coverage boilerplate** [Tier 1; a 2025+-era tell]: enumerating the *kinds* of attention a subject received — "profiled in", "independent coverage", "trade publications", "regional and national media outlets", "maintains an active social media presence", credential-stacked lists of outlet names. Press-release register echoing platform guidelines.

**Engagement-bait hooks** [Tier 1 when stacked, Tier 2 alone]: "But here's the kicker", "But here's the thing", "That's only half the story", "Here's where it gets interesting", "Picture this:", "Let that sink in.", "Read that again."

**Self-answered rhetorical hook** [Tier 1 if repeated]: "The result? Devastating." "The best part? It's free."

**Announced tone** [Tier 2 — weak alone, strong as ritual]: narrating the intended tone instead of having it — "I'm going to be straightforward about this", "Here it is, no fluff", "No sugar coating." A second-generation tell born from anti-sycophancy prompting. Humans say "let me be clear" too; the signal is the stock anti-sycophancy formula, especially when the announcement precedes content that isn't actually blunt. Fix: delete the announcement, keep (and if needed sharpen) the content.

**Vague attribution and source-count inflation** [Tier 2]: "Experts argue...", "Observers note...", "Several publications have noted..." while citing one or two; "such as" implying a longer list the sources don't support.

**LLM-safe truths and faux balance** [Tier 2]: unfalsifiable filler no rater could mark wrong ("Consistency is important", "Every business is different") and both-sides gestures that weigh nothing ("Of course, results may vary. That said..." — then proceeds unchanged).

**Formula conclusions** [Tier 2]: the rigid closing template — "Despite its [positives], [subject] faces challenges including..." followed by a vaguely positive outlook or a "Future Prospects" section. The tell is the rigid formula, not mentioning challenges.

**False ranges** [Tier 2]: "from intimate gatherings to global movements" — a spectrum that isn't one, saying nothing concrete.

**Invented concept labels** [Tier 2]: coining an authoritative-sounding term for an ordinary observation, usually in quotes or bold: "the supervision paradox", "workload creep". If the term doesn't exist in the domain, don't invent it.

**"We/our" in single-author work** [Tier 2]: research-team voice in a personal essay or individual assignment ("Our proposed design...").

**Hedging preambles** [Style-only; Legacy as a detection tell]: "It's important to note that...", "It's worth mentioning..." — largely trained out of current models, but still always worth deleting.

**Fractal summaries** [Style-only for current models; Legacy as detection]: preview, state, summarize at every level. "In summary/In conclusion" closers are now filed as historical indicators, but the fix (say it once) remains sound editing.

## Vocabulary Tells (era-stratified)

Single words are never load-bearing evidence. The signal is *clustering* (several list words in one passage) and *voice mismatch* (vocabulary above or below the writer's established baseline). Mind the dialect trap: "delve" is ordinary Nigerian and Indian formal English; enforcing word lists against unknown authors punishes real dialects. When *writing or rewriting*, avoid all eras' words — readers still pattern-match them as AI regardless of vintage.

**Current cluster (as of mid-2026 — revisit)** [Tier 2 as cluster]:
- *quietly* as drama-adverb before an achievement ("quietly building an empire", "the quiet rebellion of...") — the standout 2026 addition
- *genuinely* as sincerity-booster on praise or reactions
- *load-bearing*, *hinge*, *seams* as structural metaphors for ideas (outside technical contexts where they're native; community-reported Claude idiom)
- *emphasizing, highlighting, showcasing, enhance* (the GPT-5-era formal set)
- Business-coach filler (single-source list, weigh low): "this represents a shift", "this matters because", "the message lands", "compounds over time", "that's a signal", "do the work"

**Phrase-level markers** [Tier 1] — bigrams outlive single-word era churn: "complex and multifaceted", "intricate interplay", "played a crucial role", "That's a great question" (detector-corpus findings from GPT-era studies measuring roughly 70–700× over-representation).

**GPT-4o era (mid-2024–mid-2025)** [Tier 2, decaying]: align with, fostering, underscore, garner, bolster, leverage, navigate, ecosystem, seamlessly, comprehensive. (Showcasing, enhance, and highlighting carried forward into the current cluster above.)

**Legacy (2023–mid-2024)** [Legacy]: delve, tapestry, testament, landscape (abstract), boasts, nestled, vibrant, intricate, meticulous, interplay, bustling, diverse array, rich tapestry. A dense cluster of these still weakly suggests AI involvement (older-model or recycled output) — a single occurrence suggests nothing, or a dialect.

**Copula avoidance** [Tier 2]: "serves as", "stands as", "features", "represents", "marks", "constitutes", "boasts" where a human writes "is" or "has". Corpus studies measured a marked decline of plain is/are in post-2023 academic writing.

**Contested — "AI uses fancy words"**: probably inverted for current models, which *flatten* vocabulary toward common words and advise against distinctive ones. Absolute fanciness is not a tell; mismatch with the writer's own baseline is.

## Register and Tone Tells

**Therapy-speak / preemptive reassurance** [Tier 1 in informational or expository prose]: unprompted emotional scaffolding — "You're not broken", "You're not imagining it", "You're not alone", "Take a deep breath", "I want to say this gently" — reassurance against an objection nobody raised, e.g. in an article about pricing strategy. Artifact of 2025 chat-companion tuning. Scope: genuinely supportive contexts (grief, support forums) are exempt; this register is native there.

**Reader-praise escalation** [Tier 1]: praising the act of asking or noticing — "You didn't just make an observation, you described the moment. And honestly? That's rare.", "The fact that you noticed says a lot about you." The negative-parallelism frame turned on the reader as flattery. Currently the most-parodied AI register on the internet.

**Fake-casual register** [Tier 2 as cluster]: AI cosplay of internet-casual — "honestly", "straight up", "that tracks", "chef's kiss", "absolutely slaps", "it's giving X", "[x] era", frozen millennial-influencer enthusiasm, relentless positivity. Real humans get falsely accused over single phrases here ("that tracks"), so flag the *register* when it clusters, never one phrase.

**Rhetorical self-question + fragment validation** [Tier 1]: "Honestly? That's rare." "And that? That's growth." One-word question beat, short validating fragment.

**HR-speak / monotone warmth** [Tier 2]: overly courteous, friendly in a way no adult sounds, no humor or irritation anywhere.

**Promotional drift and puffery** [Tier 2]: neutral topics getting travel-guide or press-release treatment; mundane facts inflated.

**Uncontracted forms** [Style-only for your own writing]: "do not / it is" in informal prose reads stiff — fix it when writing. As a *detection* heuristic it is unacceptable: formal register without contractions is exactly how many ESL, formally schooled, and autistic writers naturally write.

## Structural Tells

**Uniform everything (the squint test)** [Tier 2]: sentences of near-identical length, paragraphs of identical height, every bullet in a list with the identical grammatical shape (all noun phrases, or all "Adjective noun: elaboration"). Human lists mix fragments and sentences; human paragraphs run one line to seven.

**One-point dilution (the compression test)** [Tier 2]: the whole piece compresses losslessly back to its one-sentence prompt — every section re-asserts the thesis with new metaphors and no new evidence, fluent everywhere and advancing nowhere. Distinct from fractal summaries (section-level restatement): this is the *document* failing to accumulate. Genre caveat: sermons, motivational and devotional writing legitimately restate a thesis. Fix is cut-only: keep the paragraph that says it best; never invent new evidence to make padding look substantive.

**Too-clean coherence** [Tier 3]: no stray or accidental detail, every example fits the argument perfectly, internal references resolve too neatly, no emotional spikes where a human would have one — and, in the other direction, fluent sentences whose logic jumps without bridges.

**Rule-of-three compulsion** [Tier 2]: exactly three items, three examples, three reasons, every time.

**Elegant variation** [Tier 2–3]: cycling synonyms to avoid repeating a term ("Soviet artistic constraints" → "constraints of socialist realism" → "state-imposed artistic norms"), an artifact of repetition penalties. Caveat: non-native writers are often *taught* to vary like this.

**Bold-term-colon bullets and over-structuring** [Tier 2, era-dependent]: every list item as "**Term**: explanation", headers and numbered lists imposed where prose would do. Newer models are vendor-tuned to suppress bold overuse, so absence proves nothing.

**Anaphora abuse** [Tier 2]: identical sentence openings repeated ("They assumed that users... They assumed that developers...").

**Emoji as formatting** [Tier 1 in essays, docs, and professional prose; not a tell in genuinely casual registers]: emoji prefixing every header or bullet (🚀🧠✅), plus the social-slop template — bolded hook line, sparkle clusters, "Let's connect!" CTA. The *template* is the tell — expressive emoji in a casual register are just how people write.

**Fiction stock imagery** [Tier 2, creative writing]: air smelling of "ozone and burnt sugar", rooms tilting, expressions softening, knuckles whitening; the flicker/fade/echo/static lexicon; metaphors that are uniformly trite.

## Discourse Tells

- **Regression to the mean** [Tier 2]: statistically rare specifics smoothed into statistically common grandeur — "inventor of the first train-coupling device" becomes "a revolutionary titan of industry". Less specific and more exaggerated at once.
- **RAG-era gap speculation** [Tier 1]: "While specific details are limited/scarce...", "based on available information...", "...in the provided search results..." followed by speculation about what the missing information "likely" is ("likely supports diverse wildlife", "maintains a low profile").
- **Perspective collapse** [Tier 2]: aggregating too many viewpoints instead of holding one; acknowledging alternatives is human, having no viewpoint is not.
- **Register rigidity and emotional flatness** [Tier 3]: uniform formality and smooth warm monotone throughout; corpus studies also found reduced auditory/tactile language versus human writing.
- **Citation forensics** [Tier 0–1 when verifiable]: plausibly-wrong sources — right scholar, wrong work; DOIs resolving to unrelated papers; ISBN checksum failures; book citations without page numbers; placeholder citation fields. Counter-tell: *unsourced* content is NOT an AI indicator — modern LLMs cite plenty; the citations are just wrong.

## Typographic Tells

**Em dashes** [Tier 3 alone; Tier 2 as pattern]: the most famous tell and now one of the weakest. What survives diagnostically is the combination: high density (several per paragraph) + *spaced* em dashes (" — ", contrary to typographic convention) + punched-up sales cadence. Presence alone proves nothing — a public backlash of false accusations made professional writers abandon the mark, and OpenAI made suppression reliably controllable in late 2025, so newer GPT output often has none while Claude/Gemini retain them (a weak model-family hint, nothing more).

**Hyphen-swap laundering** [Tier 2]: an em-dash-shaped clause punctuated with a plain hyphen ("the design - not the code - is the problem") reads as someone mechanically stripping em dashes from AI output. Second-order detection of first-order evasion.

**Curly quotes** [Tier 3, scoped]: only meaningful as *inconsistency* — curly and straight marks mixed in one text, or curly marks in a plain-text medium that produces straight ones. Word processors, macOS, and citation tools produce curly quotes legitimately. Weak model fingerprint: ChatGPT/DeepSeek typically curly, Gemini/Claude typically straight.

**Title case headings** [Tier 3]: chatbots strongly prefer Title Case for every main word; conspicuous where the document's convention is sentence case, meaningless where title case is the norm.

**Uniform American English** [Tier 3]: defaulting to American spelling even where context calls for British/other variety.

## Do Not Flag

Formally debunked indicators. Never cite these as evidence of AI authorship, alone or as a review's basis:

- Perfect grammar or spelling, in isolation — polished humans, Grammarly users, and careful ESL writers produce it constantly
- Formal or academic register in general; absence of contractions
- Transition words ("however", "moreover") in isolation — only the specific overused items in this catalog count, and weakly
- Semicolons — people have been "convicted" of AI over one grammatically correct semicolon
- Absence of typos
- "Bland", "robotic", or "soulless" vibes — average readers distinguish AI from human text at roughly chance; only heavy LLM users do meaningfully better
- Unsourced content — modern LLMs cite; the citations are just often wrong
- Mixed casual/formal registers — indicates multiple authors, neurodivergence, or ordinary humanity

The stakes: GPT detectors flagged the majority of genuine non-native TOEFL essays as AI in a 2023 Stanford study, and misclassification of autistic writers' precise, formulaic style is documented. These patterns are *compatible with human authorship* signals, not proof of anything: plain is/has, superlatives and definitive claims, hedging intensifiers ("very", "perhaps"), wordy constructions ("in order to"), typos and "edit:" addenda. They carry no evidential weight either way — do not cite them to clear a text, do not "fix them away" as if they were tells, and NEVER insert them to fake humanity (see the anti-patterns section of [reformulation-guide.md](reformulation-guide.md)).

## Legacy Indicators (2022-2024)

Useful for dating text or finding old undetected AI content; weak evidence about current output.

- "As an AI language model..." refusals; abrupt mid-sentence cutoffs
- Didactic disclaimers: "It's important to note...", jurisdiction-varies caveats, safety advice descended from partial refusals
- Compulsive "In summary / In conclusion / Overall" section closers
- The delve/tapestry vocabulary cluster (see Vocabulary Tells)
- Fixed knowledge-cutoff disclaimers ("as of my last knowledge update") — the RAG-era descendant ("details are limited in available sources...") is current, see Discourse Tells

## Model Idiolects

Model families are separable by style — commercial detectors classify *which* model wrote a text. Rough fingerprints (as of mid-2026): GPT-5.x — suppressed em dashes, drier register, emphasizing/highlighting set, notability boilerplate; Claude — "genuinely", "That's a great question", structural metaphors (load-bearing/hinge/seams), em-dash retention, decorative emoji in creative output; Gemini — double-adjective pairings ("a quiet, deliberate rebellion"), heavy negative parallelism, straight quotes; Grok — pseudo-scientific vocabulary (causal, empirical, correlate), "underscore", "X rather than Y". Use for dating/attribution context, not as primary evidence.

## Sources and Staleness

As of July 2026. Anchors: [Wikipedia:Signs of AI writing](https://en.wikipedia.org/wiki/Wikipedia:Signs_of_AI_writing) (living field guide; the counter-list and era tables largely mirror it); Kobak et al., *Science Advances* 2025 (excess-vocabulary effect sizes across 15M abstracts); Liang et al., *Patterns* 2023 (detector bias against non-native writers); DAMAGE, arXiv:2501.03437 (humanizer-output detection); 2025 detection studies (average readers ≈ chance, heavy LLM users ≈ 90%). All frequency claims are model- and era-specific measurements, not permanent facts. LLM style is also leaking into genuine human writing, raising every tell's human base rate over time — one more reason clusters, not single hits, carry the weight.
