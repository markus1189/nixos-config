import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  pi.on("before_agent_start", async (event, ctx) => {
    if (process.env.MH_CLAUDE_USE_GLADOS === "1") {
      return {
        message: {
          customType: "mh-glados-mode",
          content: `<system-reminder>
@gladosPrompt@
</system-reminder>`,
          display: false,
        },
      };
    }
  });
}
