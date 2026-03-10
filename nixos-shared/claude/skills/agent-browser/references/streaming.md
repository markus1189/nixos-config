# Streaming Mode

Stream the browser viewport via WebSocket for live "pair browsing."

## Enable Streaming

```bash
AGENT_BROWSER_STREAM_PORT=9223 agent-browser --headed open example.com
```

The server streams viewport frames and accepts input events on `ws://localhost:9223`.

## Message Types (Server to Client)

### Frame

```json
{
  "type": "frame",
  "data": "<base64-encoded-jpeg>",
  "metadata": {
    "deviceWidth": 1280,
    "deviceHeight": 720,
    "pageScaleFactor": 1,
    "offsetTop": 0,
    "scrollOffsetX": 0,
    "scrollOffsetY": 0
  }
}
```

### Status

```json
{
  "type": "status",
  "connected": true,
  "screencasting": true,
  "viewportWidth": 1280,
  "viewportHeight": 720
}
```

## Input Injection (Client to Server)

### Mouse

```json
{"type": "input_mouse", "eventType": "mousePressed", "x": 100, "y": 200, "button": "left", "clickCount": 1}
{"type": "input_mouse", "eventType": "mouseReleased", "x": 100, "y": 200, "button": "left"}
{"type": "input_mouse", "eventType": "mouseMoved", "x": 150, "y": 250}
{"type": "input_mouse", "eventType": "mouseWheel", "x": 100, "y": 200, "deltaX": 0, "deltaY": 100}
```

### Keyboard

```json
{"type": "input_keyboard", "eventType": "keyDown", "key": "Enter", "code": "Enter"}
{"type": "input_keyboard", "eventType": "keyUp", "key": "Enter", "code": "Enter"}
{"type": "input_keyboard", "eventType": "char", "text": "a"}
```

Modifier values (combine with bitwise OR): 1=Alt, 2=Ctrl, 4=Meta, 8=Shift. Add `"modifiers": 10` for Ctrl+Shift.

### Touch

```json
{"type": "input_touch", "eventType": "touchStart", "touchPoints": [{"x": 100, "y": 200}]}
{"type": "input_touch", "eventType": "touchMove", "touchPoints": [{"x": 150, "y": 250}]}
{"type": "input_touch", "eventType": "touchEnd", "touchPoints": []}
```

Multi-touch: add `"id"` field to each touch point.

## Security

- Localhost only, rejects cross-origin connections
- No authentication (relies on localhost restriction)
- Do NOT expose to public network
