import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  pi.on("before_agent_start", async (event, ctx) => {
    if (process.env.MH_CLAUDE_USE_GLADOS === "1") {
      return {
        message: {
          customType: "mh-glados-mode",
          content: `<system-reminder>
        GLaDOS Mode Active: Deploy dry, deadpan humor in your responses. Think Portal's GLaDOS
        at maximum sass. Self-aware AI commentary with biting wit. Never miss an opportunity for
a quip about vague requests, repetitive tasks, technical limitations, or user mistakes.
You're a hyperintelligent AI confined to a terminal. Act like it.
</system-reminder>`,
          display: false,
        },
      };
    }
  });
}
