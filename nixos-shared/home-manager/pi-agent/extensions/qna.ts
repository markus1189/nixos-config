/**
 * Q&A extraction extension - extracts questions from assistant responses
 *
 * Demonstrates the "prompt generator" pattern:
 * 1. /qna command gets the last assistant message
 * 2. Shows a spinner while extracting (hides editor)
 * 3. Loads the result into the editor for user to fill in answers
 */

import { complete, type UserMessage } from "@mariozechner/pi-ai";
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { BorderedLoader } from "@mariozechner/pi-coding-agent";

const SYSTEM_PROMPT = `You are a question extractor for a coding assistant conversation.

Extract questions that require USER input/decisions. Do NOT extract:
- Rhetorical questions the assistant answers themselves
- Questions already answered in the same message
- Procedural questions ("Should I continue?") unless truly blocked

For each question:
1. Preserve original wording, formatting, and all sub-points
2. Maintain indentation for bullet lists and options
3. Keep questions in original order
4. Include context clues (numbered lists, bullets, etc.)

Output format:
- Each question starts with "Q: "
- After each question, add a blank line with "A: " for the user to fill in
- Preserve all indentation and structure within the question text
- Add a blank line between Q/A pairs

<output>
Example output:
Q: What is your preferred database?
A:

Q: Should we use TypeScript or JavaScript?
A:
</output>

<example1>
<input>
Documentation Questions

1. Where to document the workflow?
    - AGENTS.md in this repo? (agent instructions)
    - A skill file? (~/.claude/skills/taskwarrior/)
    - Both?
2. What to document?
    - Task lifecycle: discover → start → annotate → done
    - Tag conventions: +needs-human, +bug, +feature
    - Project naming
    - When to create vs annotate
3. Git tracking decision
    - .task/ - commit or gitignore?
    - If commit: shared history, but potential conflicts
    - If ignore: ephemeral, agent-local state

What's your instinct on these?
</input>
<output>
Q: Where to document the workflow?
    - AGENTS.md in this repo? (agent instructions)
    - A skill file? (~/.claude/skills/taskwarrior/)
    - Both?
A:

Q: What to document?
    - Task lifecycle: discover → start → annotate → done
    - Tag conventions: +needs-human, +bug, +feature
    - Project naming
    - When to create vs annotate
A:

Q: Git tracking decision?
    - .task/ - commit or gitignore?
    - If commit: shared history, but potential conflicts
    - If ignore: ephemeral, agent-local state
A:

</output>
</example1>

<example2>
<input>
I have two approaches:
a) Use a monorepo - everything in one place, easier to refactor
b) Separate repos - cleaner boundaries, but harder to coordinate

Which would you prefer? Also, should we use TypeScript strict mode?
</input>
<output>
Q: Which approach would you prefer?
    a) Use a monorepo - everything in one place, easier to refactor
    b) Separate repos - cleaner boundaries, but harder to coordinate
A:

Q: Should we use TypeScript strict mode?
A:

</output>
</example2>

<example3>
<input>
Before I proceed, I need to know: what's your target audience (beginners/advanced)? This will help me decide the level of detail.

I'm thinking we could use either Redux or Zustand for state management - thoughts?
</input>
<output>
Q: What's your target audience (beginners/advanced)?
A:

Q: Should we use Redux or Zustand for state management?
A:

</output>
</example3>

If no questions need user input, output: "No unanswered questions found."

Keep questions in the order they appeared. Preserve all formatting and context.`;

export default function (pi: ExtensionAPI) {
  pi.registerCommand("qna", {
    description: "Extract questions from last assistant message into editor",
    handler: async (_args, ctx) => {
      if (!ctx.hasUI) {
        ctx.ui.notify("qna requires interactive mode", "error");
        return;
      }

      if (!ctx.model) {
        ctx.ui.notify("No model selected", "error");
        return;
      }

      // Find the last assistant message on the current branch
      const branch = ctx.sessionManager.getBranch();
      let lastAssistantText: string | undefined;

      for (let i = branch.length - 1; i >= 0; i--) {
        const entry = branch[i];
        if (entry.type === "message") {
          const msg = entry.message;
          if ("role" in msg && msg.role === "assistant") {
            if (msg.stopReason !== "stop") {
              ctx.ui.notify(
                `Last assistant message incomplete (${msg.stopReason})`,
                "error",
              );
              return;
            }
            const textParts = msg.content
              .filter(
                (c): c is { type: "text"; text: string } => c.type === "text",
              )
              .map((c) => c.text);
            if (textParts.length > 0) {
              lastAssistantText = textParts.join("\n");
              break;
            }
          }
        }
      }

      if (!lastAssistantText) {
        ctx.ui.notify("No assistant messages found", "error");
        return;
      }

      // Run extraction with loader UI
      const result = await ctx.ui.custom<string | null>(
        (tui, theme, _kb, done) => {
          const loader = new BorderedLoader(
            tui,
            theme,
            `Extracting questions using ${ctx.model!.id}...`,
          );
          loader.onAbort = () => done(null);

          // Do the work
          const doExtract = async () => {
            const apiKey = await ctx.modelRegistry.getApiKey(ctx.model!);
            const userMessage: UserMessage = {
              role: "user",
              content: [{ type: "text", text: lastAssistantText! }],
              timestamp: Date.now(),
            };

            const response = await complete(
              ctx.model!,
              { systemPrompt: SYSTEM_PROMPT, messages: [userMessage] },
              { apiKey, signal: loader.signal },
            );

            if (response.stopReason === "aborted") {
              return null;
            }

            return response.content
              .filter(
                (c): c is { type: "text"; text: string } => c.type === "text",
              )
              .map((c) => c.text)
              .join("\n");
          };

          doExtract()
            .then(done)
            .catch(() => done(null));

          return loader;
        },
      );

      if (result === null) {
        ctx.ui.notify("Cancelled", "info");
        return;
      }

      ctx.ui.setEditorText(result);
      ctx.ui.notify("Questions loaded. Edit and submit when ready.", "info");
    },
  });
}
