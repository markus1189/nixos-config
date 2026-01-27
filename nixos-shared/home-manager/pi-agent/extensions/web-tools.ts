/**
 * Web Tools Extension
 *
 * Provides web search and web fetch tools using local CLI utilities:
 * - web_search: Search the web using ddgr (DuckDuckGo CLI)
 * - web_fetch: Fetch and convert web pages to markdown using curl + pandoc
 *
 * Requirements:
 * - ddgr: `nix-shell -p ddgr` or install via package manager
 * - curl: typically pre-installed
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
} from "@mariozechner/pi-coding-agent";
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

    async execute(_toolCallId, params, onUpdate, _ctx, signal) {
      const { query, num_results = 10 } = params as {
        query: string;
        num_results?: number;
      };

      onUpdate?.({
        content: [{ type: "text", text: `Searching: ${query}...` }],
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

    async execute(_toolCallId, params, onUpdate, _ctx, signal) {
      const { url } = params as { url: string };

      onUpdate?.({
        content: [{ type: "text", text: `Fetching: ${url}...` }],
      });

      // Create a temp file for the pipeline
      const tempFile = path.join(
        os.tmpdir(),
        `pi-web-fetch-${Date.now()}.html`,
      );

      try {
        // Fetch and convert in a single pipeline using bash
        // curl -> pandoc to avoid needing stdin support
        const result = await pi.exec(
          "bash",
          [
            "-c",
            `curl -sL -A 'Mozilla/5.0 (compatible; pi-agent/1.0)' --max-time 30 --max-filesize 5000000 ${shellEscape(url)} | pandoc -f html -t gfm-raw_html --wrap=none`,
          ],
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
