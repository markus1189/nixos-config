import { spawn } from "child_process"

const CHECK_SCRIPT = "@checkScript@"

// The backend exits 2 on a match and explains the rule on stderr.
const BACKEND_BLOCKED = 2

function check(command: string): Promise<string | undefined> {
  return new Promise((resolve) => {
    const child = spawn(CHECK_SCRIPT, [], { timeout: 5000 })
    let stderr = ""
    child.stderr.on("data", (chunk) => (stderr += chunk))
    child.on("error", () => resolve(undefined))
    child.on("close", (status) =>
      resolve(status === BACKEND_BLOCKED ? stderr.trim() || "Blocked: dangerous command." : undefined),
    )
    child.stdin.end(JSON.stringify({ tool_name: "Bash", tool_input: { command } }))
  })
}

// A deny from a config `permissions` rule short-circuits before this hook and
// reaches the model as a bare "Permission denied: shell" (OpenCode 2.1.2);
// denying here is the only way the backend's explanation becomes the reason.
//
// OpenCode splits a shell call into its simple commands and passes each as one
// resource, so the backend never sees the `&&`/`;` around them.
export default {
  id: "check-dangerous-commands",
  async setup(ctx: any) {
    await ctx.permission.hook("evaluate", async (h: any) => {
      if (h.action !== "shell") return
      for (const resource of h.resources) {
        const reason = await check(resource)
        if (reason) {
          h.effect = "deny"
          h.message = reason
          return
        }
      }
    })
  },
}
