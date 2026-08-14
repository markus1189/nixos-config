/**
 * Canned Responses Extension
 *
 * alt+r opens a picker of canned responses; picking one sends it immediately
 * as a user message, triggering the agent turn.
 *
 * Supports single-letter shortcuts (e.g. "g", "e", "r") directly in the picker
 * to immediately select and send the canned response without navigation/Enter.
 *
 * Also registers global keyboard shortcuts (e.g. alt+shift+g / alt+g / etc if needed,
 * or within the alt+r popup picker) for direct triggering.
 */

import type {
  ExtensionAPI,
  ExtensionContext,
} from "@earendil-works/pi-coding-agent";
import {
  Container,
  Key,
  matchesKey,
  Text,
  truncateToWidth,
  visibleWidth,
} from "@earendil-works/pi-tui";

interface CannedResponse {
  shortcut: string;
  label: string;
  message: string;
}

const RESPONSES: CannedResponse[] = [
  {
    shortcut: "g",
    label: "Go ahead",
    message: "Go ahead",
  },
  {
    shortcut: "c",
    label: "Commit this",
    message: "Commit this",
  },
  {
    shortcut: "e",
    label: "Explore",
    message: "Explore options, explain tradeoffs",
  },
  {
    shortcut: "r",
    label: "Rate",
    message: "Rate the options based on a set of criteria you choose according to relevance",
  },
];

export default function (pi: ExtensionAPI) {
  pi.registerShortcut("alt+r", {
    description: "Open canned responses picker (supports shortcut letters)",
    handler: async (ctx: ExtensionContext) => {
      if (!ctx.isIdle()) {
        ctx.ui.notify(
          "Canned responses unavailable while streaming",
          "warning",
        );
        return;
      }

      const selected = await ctx.ui.custom<CannedResponse | null>(
        (tui, theme, _kb, done) => {
          let selectedIndex = 0;
          let cachedLines: string[] | undefined;

          function refresh() {
            cachedLines = undefined;
            tui.requestRender();
          }

          function handleInput(data: string) {
            // Direct single-letter shortcut triggers
            const lower = data.toLowerCase();
            const matched = RESPONSES.find(
              (r) => r.shortcut.toLowerCase() === lower,
            );
            if (matched) {
              done(matched);
              return;
            }

            // Arrow / vim navigation
            if (matchesKey(data, Key.up) || lower === "k") {
              selectedIndex =
                selectedIndex === 0 ? RESPONSES.length - 1 : selectedIndex - 1;
              refresh();
              return;
            }
            if (matchesKey(data, Key.down) || lower === "j") {
              selectedIndex =
                selectedIndex === RESPONSES.length - 1 ? 0 : selectedIndex + 1;
              refresh();
              return;
            }

            // Enter to submit selected
            if (matchesKey(data, Key.enter)) {
              done(RESPONSES[selectedIndex] ?? null);
              return;
            }

            // Escape / Ctrl+C to cancel
            if (matchesKey(data, Key.escape) || matchesKey(data, Key.ctrl("c"))) {
              done(null);
              return;
            }
          }

          function render(width: number): string[] {
            if (cachedLines) return cachedLines;

            const lines: string[] = [];
            const renderWidth = Math.max(1, width);

            lines.push(theme.fg("accent", "─".repeat(renderWidth)));
            lines.push(
              truncateToWidth(
                ` ${theme.fg("accent", theme.bold("Canned Responses"))}`,
                renderWidth,
              ),
            );
            lines.push("");

            for (let i = 0; i < RESPONSES.length; i++) {
              const resp = RESPONSES[i];
              const isSelected = i === selectedIndex;
              const prefix = isSelected ? theme.fg("accent", "> ") : "  ";
              const keyBadge = theme.fg("accent", `[${resp.shortcut}]`);
              const labelText = isSelected
                ? theme.fg("accent", theme.bold(resp.label))
                : theme.fg("text", resp.label);
              const descText =
                resp.label !== resp.message
                  ? theme.fg("muted", ` - ${resp.message}`)
                  : "";

              lines.push(
                truncateToWidth(
                  `${prefix}${keyBadge} ${labelText}${descText}`,
                  renderWidth,
                ),
              );
            }

            lines.push("");
            lines.push(
              truncateToWidth(
                ` ${theme.fg("dim", "letter/enter select • ↑↓/jk navigate • esc cancel")}`,
                renderWidth,
              ),
            );
            lines.push(theme.fg("accent", "─".repeat(renderWidth)));

            cachedLines = lines;
            return lines;
          }

          return {
            render,
            invalidate: () => {
              cachedLines = undefined;
            },
            handleInput,
          };
        },
      );

      if (!selected) return;

      await pi.sendUserMessage(selected.message);
    },
  });
}
