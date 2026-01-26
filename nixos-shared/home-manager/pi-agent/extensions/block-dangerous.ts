/**
 * Block Dangerous Commands Extension
 *
 * Prompts for confirmation before running dangerous bash commands:
 * - sudo (privilege escalation)
 * - rm -rf and variants (recursive force delete)
 * - git commands with --force (destructive git operations)
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

interface DangerousPattern {
	pattern: RegExp;
	reason: string;
}

const dangerousPatterns: DangerousPattern[] = [
	// sudo
	{
		pattern: /\bsudo\s/,
		reason: "Running with elevated privileges (sudo)",
	},

	// rm -rf variants:
	// rm -rf, rm -fr, rm -r -f, rm -f -r
	// rm --recursive --force, rm --force --recursive
	// rm -r --force, rm --recursive -f, etc.
	{
		pattern: /\brm\s+(-[a-zA-Z]*r[a-zA-Z]*\s+-[a-zA-Z]*f[a-zA-Z]*|-[a-zA-Z]*f[a-zA-Z]*\s+-[a-zA-Z]*r[a-zA-Z]*|-[a-zA-Z]*rf[a-zA-Z]*|-[a-zA-Z]*fr[a-zA-Z]*)/,
		reason: "Recursive delete with force (rm -rf)",
	},
	{
		pattern: /\brm\s+.*--recursive.*--force|--force.*--recursive/,
		reason: "Recursive delete with force (rm --recursive --force)",
	},
	{
		pattern: /\brm\s+(-[a-zA-Z]*r[a-zA-Z]*\s+.*--force|--recursive\s+.*-[a-zA-Z]*f[a-zA-Z]*)/,
		reason: "Recursive delete with force (rm -r --force)",
	},
	{
		pattern: /\brm\s+(-[a-zA-Z]*f[a-zA-Z]*\s+.*--recursive|--force\s+.*-[a-zA-Z]*r[a-zA-Z]*)/,
		reason: "Recursive delete with force (rm -f --recursive)",
	},

	// git with --force or -f (for push, reset, checkout, etc.)
	{
		pattern: /\bgit\s+\S+.*\s--force\b/,
		reason: "Git command with --force flag",
	},
	{
		pattern: /\bgit\s+(push|reset|checkout|branch|rebase)\s+.*-[a-zA-Z]*f/,
		reason: "Git command with force flag (-f)",
	},
];

export default function (pi: ExtensionAPI) {
	pi.on("tool_call", async (event, ctx) => {
		if (event.toolName !== "bash") return;

		const command = event.input.command as string;

		for (const { pattern, reason } of dangerousPatterns) {
			if (pattern.test(command)) {
				if (!ctx.hasUI) {
					return {
						block: true,
						reason: `Dangerous command blocked (no UI): ${reason}`,
					};
				}

				const displayCmd = command.length > 100 
					? command.slice(0, 100) + "..." 
					: command;

				const choice = await ctx.ui.select(
					`⚠️  ${reason}\n\n  ${displayCmd}\n\nAllow this command?`,
					["Allow once", "Block"]
				);

				if (choice !== "Allow once") {
					return { block: true, reason: `Blocked by user: ${reason}` };
				}

				return;
			}
		}
	});
}
