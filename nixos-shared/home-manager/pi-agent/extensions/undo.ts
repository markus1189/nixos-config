/**
 * /undo — rewind the session leaf back to your Nth-previous user message.
 *
 * Thin wrapper over ctx.navigateTree() (the same primitive /tree uses),
 * running fast and transient:
 *   - transient: the /undo command never appears in the transcript
 *   - no summary: the abandoned sub-branch stays in the tree, unsummarized
 *   - unit: N counts user messages going back
 *     /undo     == rewind to previous user message
 *     /undo 3   == rewind to the 3rd user message back
 *   - no /redo, no list subcommand: forward nav stays via /tree
 *
 * Install: copy to ~/.pi/agent/extensions/undo.ts (or .pi/extensions/)
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

export default function undoExtension(pi: ExtensionAPI) {
	pi.registerCommand("undo", {
		description:
			"Rewind the session to your Nth-previous user message (default 1). Transient, no summary.",
		handler: async (args, ctx) => {
			// ---- parse N ----
			let n = 1;
			const trimmed = args.trim();
			if (trimmed !== "") {
				if (!/^\d+$/.test(trimmed)) {
					ctx.ui.notify("/undo takes a positive integer (e.g. /undo 2)", "warning");
					return;
				}
				n = parseInt(trimmed, 10);
				if (n < 1) {
					ctx.ui.notify("/undo needs a step >= 1", "warning");
					return;
				}
			}

			// ---- walk back to the Nth user message ----
			// getBranch() returns ROOT->leaf; we must walk in reverse (leaf->root)
			const branch = ctx.sessionManager.getBranch(); // root -> leaf
			let found: { id: string; text: string } | null = null;

			for (let i = branch.length - 1; i >= 0; i--) {
				const entry = branch[i];
				// only count real user messages; skip tool/assistant/compaction/branch_summary
				if (
					entry.type === "message" &&
					entry.message?.role === "user" &&
					!entry.message.contentTextLoader // guard: skip if content is a loader? (optional)
				) {
					// cheap guard against the undo message itself if it were recorded
					const text = entry.message.content;
					if (text === "/undo" || (typeof text === "string" && text.startsWith("/undo"))) {
						continue;
					}
					if (--n === 0) {
						found = { id: entry.id, text: describe(entry.message.content) };
						break;
					}
				}
			}

			if (!found) {
				ctx.ui.notify(
					args.trim() === "" || n === 0
						? "Nothing to undo — already at the start of the session"
						: `Only found fewer than ${Math.abs(n)} user messages to rewind past — stuck at the session start`,
					"warning",
				);
				return;
			}

			// ---- move the leaf ----
			await ctx.navigateTree(found.id, { summarize: false });
			ctx.ui.notify(`Rewound to: ${found.text}`, "info");
		},
	});
}

// Helpful one-line preview of a user message for the notification
function describe(content: unknown): string {
	let s = "";
	if (typeof content === "string") s = content;
	else if (Array.isArray(content)) {
		s = content
			.map((c: any) => (c && typeof c.text === "string" ? c.text : ""))
			.filter(Boolean)
			.join(" ");
	}
	s = s.replace(/\s+/g, " ").trim();
	return s.length > 70 ? s.slice(0, 67) + "…" : (s || "(empty message)");
}