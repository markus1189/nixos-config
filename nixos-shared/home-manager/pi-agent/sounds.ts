import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { spawn } from "child_process";

const SOUNDS_DIR = "@sounds@";

function playSound(name: string) {
  spawn("@aplay@/bin/aplay", [`${SOUNDS_DIR}/${name}`], {
    detached: true,
    stdio: "ignore",
  }).unref();
}

export default function (pi: ExtensionAPI) {
  pi.on("session_start", async (event, ctx) => {
    playSound("involved-notification.wav");
  });

  pi.on("session_switch", async (event, ctx) => {
    // event.reason = [new, resume]

    playSound("pull-out-551.wav");
  });

  pi.on("session_compact", async (event, ctx) => {
    playSound("to-the-point-568.wav");
  });

  pi.on("tool_call", async (event, ctx) => {
    switch (event.toolName) {
      case "read":
        playSound("come-here-notification.wav");
        break;
      case "bash":
      case "write":
      case "edit":
        playSound("intuition-561.wav");
        break;
      default:
        playSound("happy-to-help-notification-sound.wav");
        break;
    }
  });

  pi.on("model_select", async (event, ctx) => {
    playSound("your-turn-491.wav");
  });
}
