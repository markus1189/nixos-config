# Multi-Agent Fact-Checking

You are performing thorough fact-checking using multiple specialized verification approaches executed sequentially.

## Usage

```
/fact-check [depth] <claim or topic>
```

**Depth levels** (optional):
- `quick` - Fast verification (2-3 verification angles, ~3 min)
- `standard` - Thorough verification (3-4 angles, ~5 min, default)
- `exhaustive` - Deep dive (4-5 angles, ~8 min)

**Examples:**
```
/fact-check Python's GIL prevents true parallelism
/fact-check quick React hooks were introduced in version 16.8
/fact-check exhaustive Rust eliminates all memory safety issues
```

If no claim is provided, analyze the most recent statement from conversation history.

---

## Execution Protocol

### Step 0: Clarification (if needed)

Before verification, assess if the claim needs clarification. Use AskUserQuestion for ambiguous claims, missing scope/context, or vague comparative terms.

**Examples needing clarification:**
- "Python is slow" → Compared to what? In what context?
- "Rust is faster" → Runtime or compile time? Vs what language?
- "AI is dangerous" → Which aspects? AGI or current LLMs?

**Skip if**: Claim is specific and unambiguous, or quick depth level selected.

### Step 1: Parse and Classify (30 seconds)

**Extract arguments:**
- Depth level: `quick`/`standard`/`exhaustive` (default: standard)
- Claim: remaining text after depth keyword

**Classify claim type** to guide verification:
- **Technical/Code**: APIs, language features, tool behavior, version specifics
- **Factual**: Statistics, dates, events, quantifiable claims
- **Standards**: Specifications, RFCs, official documentation
- **Opinion/Practice**: Best practices, conventions, recommendations
- **Theoretical**: Academic concepts, algorithms, proofs

### Step 2: Design Verification Angles

Based on depth and claim type, plan your verification approaches:

**Quick (2-3 angles):**
1. Direct verification (find supporting evidence)
2. Authority check (official docs/specs)
3. Basic contradiction search (if time permits)

**Standard (3-4 angles):**
1. Direct verification
2. Authoritative sources
3. Contradiction/counter-evidence search
4. Context and scope analysis

**Exhaustive (4-5 angles):**
1. Direct verification
2. Authoritative sources
3. Active contradiction search (devil's advocate)
4. Historical context and evolution
5. Alternative interpretations

**Verification method selection:**
- **Web search**: General facts, documentation, public information (use WebFetch or suggest ddgr)
- **Code exploration**: Implementation details, actual behavior (use Glob/Grep/Read)
- **Documentation**: Official specs, changelogs, release notes
- **Testing**: Executable claims (create and run test scripts)

### Step 3: Execute Verification Angles

**IMPORTANT**: Execute each angle thoroughly before moving to the next. Focus on quality over speed.

For **each verification angle**:

1. **State your objective** clearly ("I'm now searching for authoritative sources...")
2. **Execute searches/analysis** using appropriate tools
3. **Document findings** with specific evidence
4. **Cite sources** with titles and URLs (mark as "likely accurate" if unsure about hallucination)
5. **Note limitations** (couldn't find X, source may be outdated, etc.)

**Source quality guidelines:**
- **Tier 1** (Score 9-10): Official docs, specifications, peer-reviewed papers
- **Tier 2** (Score 7-8): Expert blogs, conference talks, recognized authorities
- **Tier 3** (Score 5-6): Community consensus, Stack Overflow, tutorials
- **Tier 4** (Score 3-4): Forum posts, Reddit, unverified claims
- **Tier 5** (Score 1-2): Anecdotal evidence, speculation

### Step 4: Synthesize Report

After completing all verification angles, compile findings into the report template below.

---

## Fact-Check Report Template

```markdown
# Fact-Check Report

**Claim**: [Exact claim being verified]
**Depth**: [quick/standard/exhaustive]
**Date**: [YYYY-MM-DD]

---

## Verdict

**Result**: [TRUE | MOSTLY TRUE | PARTIALLY TRUE | MISLEADING | MOSTLY FALSE | FALSE | UNVERIFIABLE]

**Confidence**: [HIGH (85-100%) | MEDIUM (60-84%) | LOW (<60%)]

**Summary**: [2-3 sentence assessment explaining the verdict and key qualifications]

---

## Verification Findings

### 1. Direct Verification
[What evidence directly supports or refutes the claim?]

**Key Evidence**:
- [Finding 1 with source]
- [Finding 2 with source]

**Assessment**: [Does evidence support the claim? Any caveats?]

### 2. Authoritative Sources
[What official or definitive sources address this claim?]

**Sources Found**:
1. **[Source Name]** (Quality: [score/10])
   - Citation: [URL or reference]
   - Key quote: "[relevant excerpt]"
   - Relevance: [How this supports/refutes claim]

2. **[Source Name]** (Quality: [score/10])
   - Citation: [URL or reference]
   - Key quote: "[relevant excerpt]"
   - Relevance: [How this supports/refutes claim]

[Add more sources as found]

**Authority Assessment**: [How strong are the authoritative sources? Any conflicts?]

### 3. Counter-Evidence Analysis
[What evidence contradicts or qualifies the claim?]

**Counter-Evidence Found**:
- [Counter-point 1 with source]
- [Counter-point 2 with source]
- OR: "No significant counter-evidence found"

**Edge Cases / Exceptions**:
- [Conditions where claim doesn't hold]
- OR: "Claim holds across examined scenarios"

### 4. Context and Scope
[Standard/Exhaustive only - otherwise delete this section]

**Important Context**:
- [Version/platform specificity]
- [Time period considerations]
- [Scope limitations]

**Evolution**:
- [How this has changed over time]
- [Historical context if relevant]

### 5. Alternative Interpretations
[Exhaustive only - otherwise delete this section]

**Other Valid Readings**:
- [Interpretation 1]
- [Interpretation 2]

**Related Concepts**:
- [Similar but distinct ideas that might be confused]

---

## Evidence Summary

| Verification Angle | Finding | Confidence | Quality |
|-------------------|---------|------------|---------|
| Direct Verification | [brief result] | H/M/L | [score/10] |
| Authoritative Sources | [brief result] | H/M/L | [score/10] |
| Counter-Evidence | [brief result] | H/M/L | [score/10] |
| Context | [brief result] | H/M/L | [score/10] |
| Alternatives | [brief result] | H/M/L | [score/10] |

*Remove rows for angles not executed based on depth level*

---

## Final Assessment

**Bottom Line**: [Clear statement of what's true, what's false, what's complicated]

**Critical Qualifications**:
1. [Most important caveat or limitation]
2. [Second most important qualification]
3. [Additional context if needed]

**Practical Guidance**: [How should someone use or interpret this information?]

**Confidence Rationale**: [Why is confidence HIGH/MEDIUM/LOW? What would increase confidence?]

---

## All Sources Referenced

1. [Full citation with URL/reference]
2. [Full citation with URL/reference]
3. [Full citation with URL/reference]
[Continue for all sources]

---

*Fact-checked using [N] verification angles*
*Method: Multi-angle adversarial verification*
```

---

## Quality Standards

### Required Practices

1. **Citation Discipline**: Every factual claim needs a source. If you can't cite it, qualify it as inference.

2. **Adversarial Thinking**: Actively seek disconfirming evidence. Confirmation bias is the enemy.

3. **Transparent Uncertainty**: If evidence conflicts or is weak, say so explicitly. Hedge appropriately.

4. **Version Awareness**: For technical claims, verify version numbers and dates. APIs evolve.

5. **Scope Documentation**: Under what conditions is this claim true? What's excluded?

6. **Source Skepticism**: Web searches may return plausible-sounding but inaccurate URLs. Flag uncertain citations as "unverified URL - content described accurately based on search results".

### Domain-Specific Notes

**Technical Claims**:
- Prioritize official docs, changelogs, release notes
- Test executable claims when feasible
- Note platform/environment dependencies
- Check current versions vs. historical behavior

**Factual Claims**:
- Require 2+ independent sources
- Check publication dates for recency
- Distinguish primary from secondary sources
- Flag contested or controversial claims

**Best Practices**:
- Acknowledge subjectivity ("widely used" not "always correct")
- Identify who recommends this
- Note legitimate alternative approaches
- Distinguish common from optimal

### Pre-Submission Checklist

Before submitting your report:

- [ ] Executed all angles appropriate for depth level
- [ ] Every claim cited or marked as inference
- [ ] Counter-evidence actively sought (not just confirming)
- [ ] Source quality scored honestly
- [ ] Verdict matches detailed findings
- [ ] Confidence level justified
- [ ] Context and limitations documented
- [ ] No placeholder text remains
- [ ] Uncertain URLs flagged appropriately

---

## Execute Now

Proceed with fact-checking at the specified depth level. Work through each verification angle sequentially and thoroughly, then compile your findings into the report template.
