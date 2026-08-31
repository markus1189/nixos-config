import {
  copyToClipboard,
  type ExtensionAPI,
  type ExtensionContext,
} from "@earendil-works/pi-coding-agent";
import { Text } from "@earendil-works/pi-tui";

/**
 * Emulated chained keybindings for pi.
 *
 *   Leader: Ctrl+X            (temporarily replaces editor with a hint bar)
 *   Then:   y   copy last assistant message to clipboard
 *           u   undo            (chains the /undo extension command)
 *           r   reload          (chains /chained-reload -> ctx.reload())
 *           p   popout          (chains the /popout extension command)
 *           ?   reshow hints  (re-renders; stays open)
 *           esc cancel        (return to editor)
 *
 * KEY-EVENT REALITY (learned the hard way — see git log / hang):
 *   `handleInput(data)` receives the RAW terminal data, NOT a key name:
 *     - printable char 'y' arrives as "y"
 *     - Escape arrives as the byte "\x1b"  (matchesKey compares data === "\x1b")
 *     - '?' arrives as "?"
 *   The docs' `text.onKey = (key) => key === "escape"` is misleading —
 *   compare against the raw data, not "escape".
 *
 * COPY CORRECTNESS:
 *   The newest assistant entry is often a tool-only turn with zero text parts.
 *   We scan BACKWARD and skip assistant entries that have no extractable text,
 *   so we return the most recent assistant message that actually HAS text.
 *
 * OVERWRITING Ctrl+X:
 *   app.message.copy is RESERVED; a bare registerShortcut("ctrl+x") is skipped.
 *   Free the chord in ~/.pi/agent/keybindings.json:
 *        { "app.message.copy": [] }
 *   Then /reload.
 */

const HINTS = "y copy  ·  u undo  ·  r reload  ·  p popout  ·  esc cancel";
const ESC = "\x1b";

/** Text of the newest assistant message that has text, or undefined if none. */
function lastAssistantText(ctx: ExtensionContext): string | undefined {
  // getBranch() = root -> leaf = the ACTIVE chain (same source /undo and the
  // builtin /copy's projected messages draw from). Do NOT use getEntries() —
  // that returns every file entry ever appended, including undone/off-branch
  // messages, so after /undo we'd copy the abandoned text.
  const assistants = [...ctx.sessionManager.getBranch()]
    .reverse()
    .filter((e) => e.type === "message" && e.message.role === "assistant");

  for (const a of assistants) {
    if (a.type !== "message") continue;
    const m = a.message;
    // Skip aborted-and-empty.
    if (m.stopReason === "aborted" && m.content.length === 0) continue;
    const parts = Array.isArray(m.content) ? m.content : [m.content];
    let text = "";
    for (const p of parts) {
      if (p && typeof p === "object" && "text" in p && typeof p.text === "string") {
        text += p.text;
      }
    }
    text = text.trim();
    // Skip tool-only turns (no text) — find an assistant message with text.
    if (text.length > 0) return text;
  }
  return undefined;
}

/**
 * Focusable component rendered in place of the editor. Implemented directly to
 * get a real `handleInput` (Text/Container have none).
 */
class ChainedMenu {
  private text: Text = new Text(HINTS, 1, 1);

  constructor(private callbacks: Callbacks) {}

  // Called by the TUI on every keypress while focused (tui.js:616).
  handleInput(data: string): void {
    if (data === ESC) {
      this.callbacks.onCancel();
      return;
    }
    const k = data.toLowerCase();
    if (k === "y") {
      this.callbacks.onCopy();
      return;
    }
    if (k === "u") {
      this.callbacks.onUndo();
      return;
    }
    if (k === "r") {
      this.callbacks.onReload();
      return;
    }
    if (k === "p") {
      this.callbacks.onPopout();
      return;
    }
    if (k === "?") {
      // Re-render / refresh hints (already visible; just ensure a fresh render).
      this.text.invalidate();
      this.callbacks.onHelp();
      return;
    }
    // any other key: stay open, hints already shown
  }

  render(width: number): string[] {
    return this.text.render(width);
  }
  invalidate(): void {
    this.text.invalidate();
  }
}

type Callbacks = {
  onCopy: () => void;
  onUndo: () => void;
  onReload: () => void;
  onPopout: () => void;
  onCancel: () => void;
  onHelp: () => void;
};

async function openCommandMenu(
  ctx: ExtensionContext,
  pi: ExtensionAPI
): Promise<void> {
  if (ctx.mode !== "tui") {
    ctx.ui.notify("Chained keybindings require TUI mode", "error");
    return;
  }

  await ctx.ui.custom<void>((tui, theme, keybindings, done) => {
    const callbacks: Callbacks = {
      onCopy: () => {
        const t = lastAssistantText(ctx);
        if (!t) {
          ctx.ui.notify("No assistant message with text to copy yet", "warning");
          return; // stay open
        }
        copyToClipboard(t).then(
          () => {
            ctx.ui.notify("Copied last assistant message", "info");
            done();
          },
          () => {
            ctx.ui.notify("Copy failed", "error");
            done();
          }
        );
      },
      onReload: () => {
        // Chain reload through our own command (builtin /reload is not
        // dispatchable via sendUserMessage — it's handled in the interactive
        // input path). /chained-reload just calls ctx.reload().
        done(); // close hint bar first
        pi.sendUserMessage("/chained-reload", { expandPromptTemplates: true });
      },
      onPopout: () => {
        // Chain the EXISTING /popout extension command — same dispatch rules as
        // /undo: expandPromptTemplates must be true or the slash text goes to the LLM.
        done(); // close hint bar first
        pi.sendUserMessage("/popout", { expandPromptTemplates: true });
      },
      onUndo: () => {
        // Chain the EXISTING /undo extension command — no reimplementation.
        // CRITICAL: expandPromptTemplates must be true, otherwise session.prompt
        // skips the extension-command dispatch and sends "/undo" to the LLM.
        done(); // close hint bar first
        pi.sendUserMessage("/undo", { expandPromptTemplates: true });
      },
      onCancel: () => {
        done();
      },
      onHelp: () => {
        // hints are already rendered; a brief toast keeps it responsive.
        ctx.ui.notify(HINTS, "info");
      },
    };

    return new ChainedMenu(callbacks);
  });
}

export default function (pi: ExtensionAPI) {
  pi.registerShortcut("ctrl+x", {
    description: "Chained command menu",
    handler: (ctx) => void openCommandMenu(ctx, pi),
  });

  // Helper command so the menu can trigger a reload. Builtin /reload is handled
  // in the interactive input path (not via sendUserMessage), so we wrap
  // ctx.reload() here and dispatch "/chained-reload" from the menu.
  pi.registerCommand("chained-reload", {
    description: "Reload extensions, skills, prompts, themes, and context files",
    handler: async (_args, ctx) => {
      await ctx.reload();
    },
  });
}