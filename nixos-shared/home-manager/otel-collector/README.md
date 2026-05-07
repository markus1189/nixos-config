# otel-collector

Local OpenTelemetry collector for capturing Claude Code telemetry.

Runs as a home-manager `systemd.user.service` on `127.0.0.1:4317` (gRPC)
and `127.0.0.1:4318` (HTTP), writes daily-rotated NDJSON to
`~/.local/share/claude-otel/claude-code.jsonl` (30-day retention via
`rotation.max_backups` in the file exporter).

## Wiring

- Module instantiated in `laptop/home.nix` via `pkgs.callPackage`,
  registered as `systemd.user.services.otel-collector`.
- Client-side env vars live in `nixos-shared/home-manager/zsh/default.nix`
  as the `otelEnv` let-binding, prefixed onto every `c*` / `cy*` alias.
- `claude-remote-control` in `laptop/home.nix` carries its own copy of
  the same vars in `Service.Environment`, with
  `OTEL_SERVICE_NAME=claude-code-remote-control` to distinguish in logs.

Bare `claude` typed without an alias is **not** telemetered — by design.

## Privacy

`OTEL_LOG_USER_PROMPTS=1` and `OTEL_LOG_TOOL_DETAILS=1` are on. Files
contain every prompt, tool argument, and bash output. Permissions are
0700/0600 via `UMask=0077`. **Exclude `~/.local/share/claude-otel/`
from any backup/sync** (Syncthing, restic, etc.) before pointing them
at `~/.local/share`.

## Query examples

Format is OTLP/JSON, not the OTEL SDK pseudo-JSON.

```bash
F=~/.local/share/claude-otel/claude-code.jsonl

# Cost so far across all retained files
jq -r '.resourceMetrics[]?.scopeMetrics[]?.metrics[]?
  | select(.name=="claude_code.cost.usage")
  | .sum.dataPoints[]?.asDouble' ~/.local/share/claude-otel/*.jsonl \
  | awk '{s+=$1} END {printf "$%.4f\n", s}'

# Tokens by model and type
jq -r '.resourceMetrics[]?.scopeMetrics[]?.metrics[]?
  | select(.name=="claude_code.token.usage")
  | .sum.dataPoints[]?
  | "\((.attributes[]? | select(.key=="model") | .value.stringValue)) \((.attributes[]? | select(.key=="type") | .value.stringValue)) \(.asDouble // .asInt)"' "$F"

# Recent prompts
jq -r '.resourceLogs[]?.scopeLogs[]?.logRecords[]?
  | select(.body.stringValue=="claude_code.user_prompt")
  | .attributes[]? | select(.key=="prompt") | .value.stringValue' "$F" | tail

# Distinct event names
jq -r '.resourceLogs[]?.scopeLogs[]?.logRecords[]?.body.stringValue // empty' "$F" \
  | sort | uniq -c | sort -rn
```

## Toggling beta traces

Stable signals only by default. To enable distributed traces, add
`CLAUDE_CODE_ENHANCED_TELEMETRY_BETA=1 OTEL_TRACES_EXPORTER=otlp` to
`otelEnv` (and the systemd `Environment` list), then add a `traces`
pipeline in this module's config:

```nix
service.pipelines.traces = {
  receivers = [ "otlp" ];
  exporters = [ "file" ];
};
```
