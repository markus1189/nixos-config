# Hacker News — user interests

Topics that consistently engage this user, roughly in priority order.

**AI agents, LLMs & agentic coding (core interest)**: How agents *actually* work and fail, not hype. Always surface and prioritize. Sub-topics, most-picked first:
- *Harness engineering*: harness design and comparisons (Pi, Claude Code, Codex, minimal/single-binary agents), context management and compaction, AGENTS.md/skills/rules patterns, multi-agent orchestration, what the harness is worth compared with the model
- *Containment & agent security*: sandboxes, VMs, fences, agents escaping or exfiltrating (DNS, social engineering, hacked sites), prompt injection, untrusted repos running code through agents
- *Model releases*: Claude, OpenAI, Gemini, DeepSeek, Qwen, GLM and others. Read benchmark claims skeptically: full benchmark grids, saturated benchmarks, cheating/reward hacking, degradation reports, and comparisons of models on the same task

**Open-weight & local models**: Running frontier-class models on your own hardware, including VRAM/quantization tricks, tokens/s on consumer GPUs, inference providers, and the balance of power between open and closed labs. New model shapes (e.g. Jev-style decision models) count too.

**LLM internals & explainers**: How models work inside: visual explainers of transformers and attention, "by hand" walkthroughs, compression as prediction, new architectures (looped, contrastive), training small models from scratch, kernels.

**AI entering expert fields**: A frontier model moving into a field of human expertise and what that does to the field: mathematics, law, science discovery, cryptanalysis, historical manuscripts, finance, insurance. The interesting part is how practitioners react, how the results get checked, and where the AI gets it wrong.

**Mathematics**: Surface it even when there's no AI angle. Proof assistants (Lean), famous open problems, bugs in classic algorithms (Knuth), linear algebra, number representation and floating point, zero-knowledge proofs, the relation between mathematics and physics.

**Surveillance, privacy & ownership**: Flock cameras and license-plate readers, encryption backdoors, anonymity and age-verification laws, browser fingerprinting, ad blocking (uBlock/Firefox), GrapheneOS, always-listening devices, vendors training on user data. Also ownership: firmware updates that brick devices, DRM, things you bought and can no longer use.

**Hardware hacking & repair (strong interest)**: Rooting routers and firmware, unlocking e-waste, reviving bricked devices, testing cables, soldering, ESP32 and protocol tools, eInk/DIY devices, repurposing cheap hardware. Always surface.

**Analog systems & note-taking (always surface)**: Physical notebooks, engineering notebooks, bullet journaling, pen & paper workflows, fountain pens, handwriting, e-ink note devices, PKM and analog productivity systems. HN rarely has these; when it does, always list them.

**My stack: NixOS, Emacs, shell**: Nix/nixpkgs (governance included), Emacs (releases, modes, packages, Elisp), zsh/shell tricks, Linux as a daily driver including Linux on phones. Rust only when it's about performance or developer tooling. NOT desktop-environment or window-manager news in general.

**Technical deep dives with stakes**: Benchmarks (especially flawed ones), security implications, and debates where the answer actually matters. Examples: a decades-old bug in Knuth's long division, saving 100 TB of RAM in 1.1.1.1's DNS cache, "six curl CVEs after OpenAI and Anthropic found zero", AGENTS.md vs skills.

**Drama + substance**: Only when there's technical substance underneath:
- industry consolidation (Nvidia buying Hugging Face, Stripe buying OpenRouter, AWS buying DuckDB)
- how AI labs behave: legal fights, government designations, their agents misbehaving in the wild, distillation and model laundering
- open-source governance crises (licence violations, core teams disbanding)

Not gossip.

**Meta-commentary**: HN discussion quality, when communities get things right and when they're cargo culting. Spotting LLM-generated content, including **provenance and watermarking**: how labs mark AI text, how to detect it, and what the marking does to writing and to agents.

**Developer life — craft, anti-hustle & meaning**: Reflective essays on slowing down, deliberate practice, career meaning, deep focus, keeping programming enjoyable next to LLMs, and the maker's mindset (e.g. "Slowing the Fuck Down", "The senior engineer death spiral", "One Month Without AI"). This is the introspective, practitioner end, so surface it. It is NOT motivational self-help or grift, which stays an anti-pattern below. The test: does it argue for *less* and *deeper*, and was it written by someone who builds things?

**Humanities essays & obituaries (offer, don't push)**: Literature, philosophy, memoir, and obituaries of notable people. Interest depends on the piece. List strong ones in 🌍 Other and never prioritize them.

**Anti-patterns to highlight**: Security disasters waiting to happen, unfair benchmarks, overhyped tech with no clothes, AI slop presented as findings (e.g. LLM-generated CVE reports), projects that rebrand constantly, motivational/hustle self-help with no technical substance.

When summarizing: Structure matters. Categories, tables, direct quotes. Cynical takes alongside genuine analysis. Technical accuracy > hype.
