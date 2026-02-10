/**
 * Web Tools Extension
 *
 * Provides web search and web fetch tools using local CLI utilities:
 * - web_search: Search the web using ddgr (DuckDuckGo CLI)
 * - web_fetch: Fetch and convert web pages to markdown using curl/wget + pandoc
 *
 * Requirements:
 * - ddgr: `nix-shell -p ddgr` or install via package manager
 * - curl: typically pre-installed
 * - wget: typically pre-installed (used as fallback)
 * - pandoc: `nix-shell -p pandoc` or install via package manager
 *
 * Usage:
 * 1. Copy to ~/.pi/agent/extensions/web-tools.ts
 * 2. Or load with: pi -e ./web-tools.ts
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import {
  truncateHead,
  DEFAULT_MAX_BYTES,
  DEFAULT_MAX_LINES,
  formatSize,
  keyHint,
} from "@mariozechner/pi-coding-agent";
import { Text } from "@mariozechner/pi-tui";
import { Type } from "@sinclair/typebox";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";

interface SearchResult {
  title: string;
  url: string;
  abstract: string;
}

export default function webToolsExtension(pi: ExtensionAPI) {
  // ─────────────────────────────────────────────────────────────────────────
  // Web Search Tool (using ddgr)
  // ─────────────────────────────────────────────────────────────────────────
  pi.registerTool({
    name: "web_search",
    label: "Web Search",
    description:
      "Search the web using DuckDuckGo. Returns titles, URLs, and abstracts for matching results. Use this to find current information, documentation, articles, or any web content.",
    parameters: Type.Object({
      query: Type.String({
        description:
          "Search query. Can include site: operator (e.g., 'site:github.com typescript')",
      }),
      num_results: Type.Optional(
        Type.Number({
          description: "Number of results to return (default: 10, max: 25)",
          minimum: 1,
          maximum: 25,
        }),
      ),
    }),

    renderCall(args, theme) {
      let text = theme.fg("toolTitle", theme.bold("web_search "));
      text += theme.fg("dim", `"${args.query}"`);
      return new Text(text, 0, 0);
    },

    renderResult(result, { expanded, isPartial }, theme) {
      // Still executing
      if (isPartial) {
        return new Text(theme.fg("muted", "Searching..."), 0, 0);
      }

      const details = result.details as {
        query?: string;
        resultCount?: number;
        results?: Array<{ title: string; url: string }>;
        error?: string;
      };

      // Handle errors
      if (result.isError || details?.error) {
        return new Text(
          theme.fg("error", `✗ ${details?.error || "Search failed"}`),
          0,
          0,
        );
      }

      // No results
      if (!details?.resultCount) {
        return new Text(
          theme.fg("warning", `No results for "${details?.query || ""}"`),
          0,
          0,
        );
      }

      // Collapsed: show count + query
      let text = theme.fg("success", "✓ ");
      text += `Found ${details.resultCount} results for "${details.query}"`;

      if (!expanded) {
        text += ` ${theme.fg("muted", `(${keyHint("expandTools", "to expand")})`)}`;
        return new Text(text, 0, 0);
      }

      // Expanded: show full results with abstracts
      const content = result.content[0];
      if (content?.type === "text" && content.text) {
        // Extract just the results part (skip the header line)
        const lines = content.text.split("\n");
        const resultsStart = lines.findIndex((l) => l.match(/^\d+\./));
        if (resultsStart >= 0) {
          text += "\n" + lines.slice(resultsStart).join("\n");
        }
      }

      return new Text(text, 0, 0);
    },

    async execute(_toolCallId, params, signal, onUpdate) {
      const { query, num_results = 10 } = params as {
        query: string;
        num_results?: number;
      };

      onUpdate?.({
        content: [{ type: "text", text: `Searching: ${query}...` }],
        details: {},
      });

      try {
        // Run ddgr with JSON output
        const result = await pi.exec(
          "ddgr",
          [
            "--unsafe", // Don't check for HTTPS
            "--json", // JSON output
            "--noua", // Don't send user agent
            "--noprompt", // Non-interactive
            "-n",
            String(Math.min(num_results, 25)),
            query,
          ],
          { signal, timeout: 30000 },
        );

        if (result.code !== 0) {
          return {
            content: [
              {
                type: "text",
                text: `Search failed: ${result.stderr || "Unknown error"}`,
              },
            ],
            details: { error: result.stderr, exitCode: result.code },
            isError: true,
          };
        }

        // Parse JSON results
        let results: SearchResult[] = [];
        try {
          results = JSON.parse(result.stdout || "[]");
        } catch {
          return {
            content: [
              {
                type: "text",
                text: `Failed to parse search results: ${result.stdout}`,
              },
            ],
            details: { error: "JSON parse error", raw: result.stdout },
            isError: true,
          };
        }

        if (results.length === 0) {
          return {
            content: [
              {
                type: "text",
                text: `No results found for: ${query}`,
              },
            ],
            details: { query, resultCount: 0 },
          };
        }

        // Format results
        const formatted = results
          .map(
            (r, i) =>
              `${i + 1}. ${r.title}\n   URL: ${r.url}\n   ${r.abstract || "(no description)"}`,
          )
          .join("\n\n");

        return {
          content: [
            {
              type: "text",
              text: `Found ${results.length} results for "${query}":\n\n${formatted}`,
            },
          ],
          details: {
            query,
            resultCount: results.length,
            results: results.map((r) => ({
              title: r.title,
              url: r.url,
            })),
          },
        };
      } catch (error) {
        const message = error instanceof Error ? error.message : String(error);
        return {
          content: [{ type: "text", text: `Search error: ${message}` }],
          details: { error: message },
          isError: true,
        };
      }
    },
  });

  // ─────────────────────────────────────────────────────────────────────────
  // Web Fetch Tool (using curl + pandoc)
  // ─────────────────────────────────────────────────────────────────────────
  pi.registerTool({
    name: "web_fetch",
    label: "Web Fetch",
    description:
      "Fetch a web page and convert it to readable markdown text. Strips HTML, scripts, and styles to extract the main content. Use this after web_search to read the full content of a page.",
    parameters: Type.Object({
      url: Type.String({
        description: "URL of the web page to fetch",
      }),
    }),

    renderCall(args, theme) {
      let text = theme.fg("toolTitle", theme.bold("web_fetch "));
      // Show just the hostname for compactness
      try {
        const url = new URL(args.url);
        text += theme.fg("dim", url.hostname);
      } catch {
        text += theme.fg("dim", args.url);
      }
      return new Text(text, 0, 0);
    },

    renderResult(result, { expanded, isPartial }, theme) {
      // Still executing
      if (isPartial) {
        return new Text(theme.fg("muted", "Fetching..."), 0, 0);
      }

      const details = result.details as {
        url?: string;
        contentLength?: number;
        truncated?: boolean;
        error?: string;
      };

      // Handle errors
      if (result.isError || details?.error) {
        return new Text(
          theme.fg("error", `✗ ${details?.error || "Fetch failed"}`),
          0,
          0,
        );
      }

      // No content
      if (!details?.contentLength) {
        return new Text(
          theme.fg("warning", "No readable content extracted"),
          0,
          0,
        );
      }

      // Collapsed: show size + truncation status
      let text = theme.fg("success", "✓ ");
      text += formatSize(details.contentLength);
      if (details.truncated) {
        text += theme.fg("warning", " (truncated)");
      }

      if (!expanded) {
        text += ` ${theme.fg("muted", `(${keyHint("expandTools", "to expand")})`)}`;
        return new Text(text, 0, 0);
      }

      // Expanded: show full content
      const content = result.content[0];
      if (content?.type === "text" && content.text) {
        // Skip the "Content from URL:" header line
        const lines = content.text.split("\n");
        const contentStart = lines.findIndex((l) => !l.startsWith("Content from"));
        if (contentStart >= 0) {
          text += "\n" + lines.slice(contentStart).join("\n");
        } else {
          text += "\n" + content.text;
        }
      }

      return new Text(text, 0, 0);
    },

    async execute(_toolCallId, params, signal, onUpdate) {
      const { url } = params as { url: string };

      onUpdate?.({
        content: [{ type: "text", text: `Fetching: ${url}...` }],
        details: {},
      });

      // Create a temp file for the pipeline
      const tempFile = path.join(
        os.tmpdir(),
        `pi-web-fetch-${Date.now()}.html`,
      );

      try {
        // Try curl first, fall back to wget if curl fails.
        // Uses a realistic User-Agent, compressed encoding, and cookie support.
        const escapedUrl = shellEscape(url);
        const ua = "'Mozilla/5.0 (X11; Linux x86_64; rv:128.0) Gecko/20100101 Firefox/128.0'";
        const pandocCmd = "pandoc -f html -t gfm-raw_html --wrap=none";

        const curlCmd = [
          `curl -sL`,
          `--compressed`,
          `-A ${ua}`,
          `--max-time 30`,
          `--max-filesize 5242880`,
          `--retry 2 --retry-delay 1 --retry-max-time 15`,
          `-H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8'`,
          `-H 'Accept-Language: en-US,en;q=0.5'`,
          `-b ''`, // enable cookie engine
          escapedUrl,
        ].join(" ");

        const wgetCmd = [
          `wget -q -O -`,
          `--compression=auto`,
          `-U ${ua}`,
          `--timeout=30`,
          `--max-redirect=10`,
          `--header='Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8'`,
          `--header='Accept-Language: en-US,en;q=0.5'`,
          escapedUrl,
        ].join(" ");

        // Try curl first; if it fails (non-zero exit or empty output), fall back to wget
        const fetchScript = `
          html=$(${curlCmd} 2>/dev/null) || html=""
          if [ -z "$html" ]; then
            html=$(${wgetCmd} 2>/dev/null) || html=""
          fi
          if [ -z "$html" ]; then
            echo "FETCH_FAILED: both curl and wget failed to retrieve content" >&2
            exit 1
          fi
          printf '%s' "$html" | ${pandocCmd}
        `;

        const result = await pi.exec(
          "bash",
          ["-c", fetchScript],
          { signal, timeout: 60000 },
        );

        if (result.code !== 0) {
          return {
            content: [
              {
                type: "text",
                text: `Failed to fetch URL: ${result.stderr || "Unknown error"}`,
              },
            ],
            details: { error: result.stderr, url },
            isError: true,
          };
        }

        let content = result.stdout;

        // Clean up common noise
        content = content
          // Remove excessive blank lines
          .replace(/\n{4,}/g, "\n\n\n")
          // Remove empty links
          .replace(/\[]\([^)]*\)/g, "")
          // Remove loading placeholders
          .replace(/Loading\.\.\./g, "")
          // Trim
          .trim();

        if (!content) {
          return {
            content: [
              {
                type: "text",
                text: `Page fetched but no readable content extracted from: ${url}`,
              },
            ],
            details: { url, contentLength: 0 },
          };
        }

        // Apply truncation
        const truncation = truncateHead(content, {
          maxLines: DEFAULT_MAX_LINES,
          maxBytes: DEFAULT_MAX_BYTES,
        });

        let resultText = truncation.content;

        if (truncation.truncated) {
          resultText += `\n\n[Content truncated: showing ${truncation.outputLines} of ${truncation.totalLines} lines`;
          resultText += ` (${formatSize(truncation.outputBytes)} of ${formatSize(truncation.totalBytes)})]`;
        }

        return {
          content: [
            {
              type: "text",
              text: `Content from ${url}:\n\n${resultText}`,
            },
          ],
          details: {
            url,
            contentLength: content.length,
            truncated: truncation.truncated,
          },
        };
      } catch (error) {
        const message = error instanceof Error ? error.message : String(error);
        return {
          content: [{ type: "text", text: `Fetch error: ${message}` }],
          details: { error: message, url },
          isError: true,
        };
      } finally {
        // Clean up temp file if it exists
        try {
          if (fs.existsSync(tempFile)) {
            fs.unlinkSync(tempFile);
          }
        } catch {
          // Ignore cleanup errors
        }
      }
    },
  });

  // Log on load
  pi.on("session_start", async (_event, ctx) => {
    if (ctx.hasUI) {
      ctx.ui.notify("Web tools loaded (web_search, web_fetch)", "info");
    }
  });
}

/**
 * Shell-escape a string for use in bash commands
 */
function shellEscape(str: string): string {
  // Use single quotes and escape any existing single quotes
  return "'" + str.replace(/'/g, "'\"'\"'") + "'";
}
