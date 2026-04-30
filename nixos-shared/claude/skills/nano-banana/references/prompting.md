# Nano Banana prompting reference

Non-obvious bits from Google's official guides. Load when crafting a non-trivial image prompt.

## Universal skeleton

`[Subject] + [Action] + [Location] + [Composition] + [Style]` as a narrative paragraph, not a keyword list.

## Model-specific workflow

**Flash** — cheap and fast; iterate conversationally. Don't over-engineer the first prompt; get to 80% then ask for edits (`"make the lighting warmer"`, `"remove the man on the left"`).

**Pro** — a *thinking* model. State intent and constraints clearly, then trust the reasoning. Stacking redundant adjectives or verbose preambles can hurt; keep `temperature` at default `1.0`. Worth the price for: 2K/4K detail, in-image text, multi-subject identity preservation.

## API quirks that bite

- Size codes are uppercase — `1K`/`2K`/`4K`, never `1k`.
- The model won't reliably produce N variants in one call; loop the script for batches.
- Transparent backgrounds aren't supported — request `pure white background` and key it out.
- Negative framing (`"no cars"`) confuses the model; write the positive (`"empty street"`).

## Text in images (Pro's superpower)

1. Quote exact text: `"GLOW"`, `"10% OFF"`.
2. Describe fonts by *style*, not vendor name: `"bold white sans-serif"`, `"flowing brush script"`, `"thin minimalist serif"`.
3. Layout each block explicitly: `Top: 'X' in <style>. Middle: 'Y' in <style>. Bottom: 'Z' in <style>.`
4. Localize by appending `"translate the text into Korean and Arabic"` — write the prompt itself in English.

## Multi-image (`-i` flag)

Label each image's role, otherwise Pro guesses and you lose control:

> Use Image A as the pose reference, Image B for the outfit color palette, and Image C for the background. Combine with consistent lighting from Image C.

## High-leverage levers

| Lever | Phrasings that move the needle |
|---|---|
| Camera | `shot on Fujifilm`, `GoPro distortion`, `disposable camera flash` |
| Lens | `85mm portrait`, `low-angle f/1.8`, `macro`, `wide-angle 24mm` |
| Lighting | `golden hour backlighting`, `three-point softbox`, `chiaroscuro` |
| Grade | `1980s color film, grainy`, `cinematic muted teal` |
| Materiality | `navy blue tweed`, `brushed brass`, `worn leather` (not "suit"/"metal"/"leather") |

## Sources

- [Google Cloud: Ultimate prompting guide for Nano Banana](https://cloud.google.com/blog/products/ai-machine-learning/ultimate-prompting-guide-for-nano-banana)
- [Google Blog: Nano Banana Pro prompt tips](https://blog.google/products-and-platforms/products/gemini/prompting-tips-nano-banana-pro/)
- [Google AI: Image generation API docs](https://ai.google.dev/gemini-api/docs/image-generation)
- [Phil Schmid: Gemini 3 prompting best practices](https://www.philschmid.de/gemini-3-prompt-practices)
