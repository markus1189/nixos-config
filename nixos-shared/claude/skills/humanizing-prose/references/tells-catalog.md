# AI Writing Tells Catalog

## Contents
- [Vocabulary Tells](#vocabulary-tells)
- [Phrase Tells](#phrase-tells)
- [Structural Tells](#structural-tells)
- [Tone Tells](#tone-tells)
- [Discourse Tells](#discourse-tells)
- [Typographic Tells](#typographic-tells)

## Vocabulary Tells

Words statistically overrepresented in LLM output vs human writing (confirmed by stylometric studies and the WaPo ChatGPT corpus analysis):

**Verbs**: delve, underscore, highlight, emphasize, foster, bolster, enhance, showcase, garner, navigate, leverage, unlock, align with, encompass, cultivate, harness, illuminate, resonate, transcend, unveil, catalyze

**Adjectives**: crucial, pivotal, key (overused as adj), intricate, meticulous, vibrant, enduring, profound, groundbreaking, renowned, comprehensive, noteworthy, commendable, multifaceted, nuanced, innovative, invaluable, holistic, transformative, robust

**Nouns (abstract/metaphorical)**: landscape, tapestry, interplay, testament, ecosystem, framework, dynamic, cornerstone, bedrock, linchpin, synergy, paradigm, realm, facet, milestone, prowess, journey, resilience

**Adverbs**: meticulously, seamlessly, notably, remarkably, particularly

**Copula replacements** (AI avoids "is/has"): serves as, stands as, features, represents, marks, constitutes, boasts. Human writers use "is" and "has" freely. A >10% decrease in "is" and "are" usage has been measured in post-2023 academic writing.

**Promotional**: boasts (meaning "has"), nestled, in the heart of, diverse array, rich tapestry, natural beauty, commitment to, bustling

## Phrase Tells

**Negative parallelism** (~6% of ChatGPT messages per WaPo study):
- "It's not X, it's Y"
- "It's less about X and more about Y"
- "Not just X, but Y"

**Hedging preambles**:
- "It's important to note that..."
- "It's worth mentioning that..."
- "It should be noted that..."
- "It's worth considering that..."

**Significance inflation**:
- "...marking a pivotal moment in..."
- "...reflecting broader trends in..."
- "...setting the stage for..."
- "...stands/serves as a testament to..."
- "...a vital/significant/crucial/pivotal role..."
- "...underscores/highlights its importance..."
- "...represents a shift..."

**Balanced hedging**:
- "While X is true, Y is also important"
- "Whether you're a [beginner] or an [expert]..."

**Superficial -ing analysis** (participle phrases appended to facts):
- "...creating a lively community within its borders"
- "...contributing to the socio-economic development of the region"
- "...highlighting its enduring legacy"
- "...ensuring a seamless experience"

**Formula conclusions**:
- "Despite its [positive], [subject] faces challenges including..."
- "Despite these challenges, [optimistic future]"
- "Ultimately, ..."

**Vague attributions**:
- "Experts argue...", "Observers note...", "Industry reports suggest..."
- "It is widely regarded as..."
- "Several publications have noted..." (citing 1-2 sources)

**Self-answered rhetorical questions**:
- "The result? Devastating."
- "Here's the kicker: ..."
- "Here's the thing: ..."
- "So what does this mean? It means..."

**Synonym cycling and false ranges**:
- Restating the same concept with different words to pad length
- "From X to Y" where X and Y aren't on any real spectrum

**Fractal summaries**:
- Previewing what you'll say, saying it, then summarizing what you said
- Happens at paragraph, section, and document level simultaneously
- Human writers say things once and trust the reader

**Invented concept labels**:
- Fabricating compound terms that sound analytical: "supervision paradox," "acceleration trap," "workload creep"
- If the term doesn't exist in the domain, don't invent it

## Structural Tells

- **Uniform sentence length**: AI maintains eerily consistent cadence; humans vary wildly
- **Uniform paragraph length**: each paragraph roughly same size
- **Uniform information density**: every paragraph carries equal weight; human writing varies from dense data to sparse reflection
- **Frictionless transitions**: no digressions, interruptions, tonal shifts
- **Textbook paragraph pattern**: topic sentence, support, concluding observation, every time
- **Over-structuring**: numbered lists and headers imposed where flowing prose would be natural
- **Bold-first bullet points**: every list item starting with **Term**: explanation
- **Balanced clauses**: diplomatic symmetry in every comparison
- **Rule of Three compulsion**: defaulting to exactly three items in every list, three examples, three reasons. Humans use two, four, or five items naturally.
- **Anaphora abuse**: repeating identical sentence openings ("They assumed that users... They assumed that developers... They assumed that ecosystems...")
- **Perfect grammar throughout**: no fragments, no conjunction-initial sentences, no run-ons, zero spelling variation. Human writing includes deliberate imperfection.

## Tone Tells

- **Uniformly positive/neutral sentiment** with no abrupt emotional modulation. LLMs show 114-133% increases in positive sentiment and suppress negative emotions.
- **HR-speak**: overly courteous, friendly in a way no adult actually sounds
- **No authentic voice**: missing humor, sarcasm, frustration, tangents, strong opinions
- **Diplomatic to a fault**: avoids commitment, always presents "both sides"
- **Promotional drift**: even neutral topics get travel-guide or press-release treatment
- **Puffery**: inflating significance of mundane facts
- **No contractions**: "do not" instead of "don't," "it is" instead of "it's"

## Discourse Tells

These operate above the sentence level and are harder to fix with word swaps.

- **Entity coherence**: human text refers back to previously introduced entities across the full document; AI clusters same-entity mentions close together, then drops them
- **Register rigidity**: AI maintains uniform formality throughout; human writing shifts register during asides, explanations, and reflections
- **Emotional flatness**: AI prose averages millions of voices into a smooth, warm monotone. Human emotional writing shifts through genuine states.
- **Sensory asymmetry**: AI favors visual references and shows 19-49% reduction in auditory and tactile language
- **No cognitive load artifacts**: human writing shows working memory constraints (false starts, complexity variation, self-corrections); AI is uniformly polished
- **Perspective collapse**: AI aggregates too many viewpoints instead of maintaining a consistent perspective. Human writers commit to a viewpoint even when acknowledging alternatives.

## Typographic Tells

- **Em dash overuse**: used constantly, especially by OpenAI models. OpenAI added a personalization setting in Nov 2025 but the default behavior persists.
- **Curly quotes**: vs straight quotes
- **Single-char ellipsis**: one Unicode character vs three dots
- **Markdown artifacts**: \*\*bold\*\*, \*italic\* in non-Markdown contexts
- **Consistent American English**: AI defaults to American spelling throughout even when the context calls for British English
