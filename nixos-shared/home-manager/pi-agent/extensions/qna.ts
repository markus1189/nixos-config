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

const SYSTEM_PROMPT = `
Task: Extract questions/suggestions/options/enumerated choices/offers to continue from a message.

Role: Professional extractor, able to condense it down to the essence without losing information.

Constraints:
- Folow the exact output format.
- No other markup, just plain text as given.

<output-format>
<output-questions>
Q1: <question1> - rationale/options/context
A1:

Q2: Which option do you choose?
- Option 1: $description
- Option N: $description

A2:
</output-questions>
</output-format>

<examples>
<example1>
<input>
... analysis, rationale, etc

Would you like me to:
 1. Fix the commit message to include the Jira ticket?
 2. Review any implementation code changes?
 3. Help prepare the implementation PR?
</input>
<output>
Q1: Fix the commit message to include the Jira ticket? (y/n)
A1:

Q2: Review any implementation code changes? (y/n)
A2:

Q3: Help prepare the implementation PR? (y/n)
A3:
</output>
</example1>
<example2>
<input>
...
Next Steps:
 1. ✅ API spec ready for PR review
 2. ⚠️ Implementation blockers need resolution before coding:
     - Clarify delivery reservation scope (per-item vs per-checkout)
     - Define phone parsing strategy
     - Define isHome logic

 Would you like me to help with any of these implementation decisions or prepare questions for the product/business team?
</input>
<output>
Q1: What next steps would you like to follow up with?
- Option 1: Clarify delivery reservation scope (per-item vs per-checkout)
- Option 2: Define phone parsing strategy
- Option 3: Define isHome logic
A1:
</output>
</example2>
</examples>

Possible examples are next possible steps, enumeration of things one
could to, asking for the choice from a set of options, etc.  Basically
anything that indicates it's possible to answer.

Return only the output format, nothing else.

If you don't find anything, explain briefly why.
`;

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
