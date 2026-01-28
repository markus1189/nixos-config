# Streaming Mode

Stream the browser viewport via WebSocket for live preview or "pair browsing" where a human can watch and interact alongside an AI agent.

## Enable Streaming

Set the `AGENT_BROWSER_STREAM_PORT` environment variable:

```bash
AGENT_BROWSER_STREAM_PORT=9223 agent-browser open example.com
```

The server:
- Streams viewport frames as base64-encoded images
- Accepts input events (mouse, keyboard, touch)
- Runs on `ws://localhost:9223`

## WebSocket Protocol

### Connect

```javascript
const ws = new WebSocket('ws://localhost:9223');

ws.onmessage = (event) => {
  const data = JSON.parse(event.data);
  
  if (data.type === 'frame') {
    // Display frame
    const img = new Image();
    img.src = 'data:image/jpeg;base64,' + data.data;
    document.body.appendChild(img);
  }
  
  if (data.type === 'status') {
    console.log('Connected:', data.connected);
    console.log('Screencasting:', data.screencasting);
  }
};
```

## Message Types

### Frame Messages (Server → Client)

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

### Status Messages (Server → Client)

```json
{
  "type": "status",
  "connected": true,
  "screencasting": true,
  "viewportWidth": 1280,
  "viewportHeight": 720
}
```

## Input Injection (Client → Server)

### Mouse Events

**Click:**
```json
{
  "type": "input_mouse",
  "eventType": "mousePressed",
  "x": 100,
  "y": 200,
  "button": "left",
  "clickCount": 1
}
```

**Release:**
```json
{
  "type": "input_mouse",
  "eventType": "mouseReleased",
  "x": 100,
  "y": 200,
  "button": "left"
}
```

**Move:**
```json
{
  "type": "input_mouse",
  "eventType": "mouseMoved",
  "x": 150,
  "y": 250
}
```

**Scroll:**
```json
{
  "type": "input_mouse",
  "eventType": "mouseWheel",
  "x": 100,
  "y": 200,
  "deltaX": 0,
  "deltaY": 100
}
```

### Keyboard Events

**Key Down:**
```json
{
  "type": "input_keyboard",
  "eventType": "keyDown",
  "key": "Enter",
  "code": "Enter"
}
```

**Key Up:**
```json
{
  "type": "input_keyboard",
  "eventType": "keyUp",
  "key": "Enter",
  "code": "Enter"
}
```

**Type Character:**
```json
{
  "type": "input_keyboard",
  "eventType": "char",
  "text": "a"
}
```

**With Modifiers:**
```json
{
  "type": "input_keyboard",
  "eventType": "keyDown",
  "key": "c",
  "code": "KeyC",
  "modifiers": 2
}
```

Modifier values:
- 1 = Alt
- 2 = Ctrl
- 4 = Meta (Cmd)
- 8 = Shift

Combine with bitwise OR: `2 | 8 = 10` (Ctrl+Shift)

### Touch Events

**Touch Start:**
```json
{
  "type": "input_touch",
  "eventType": "touchStart",
  "touchPoints": [{ "x": 100, "y": 200 }]
}
```

**Touch Move:**
```json
{
  "type": "input_touch",
  "eventType": "touchMove",
  "touchPoints": [{ "x": 150, "y": 250 }]
}
```

**Touch End:**
```json
{
  "type": "input_touch",
  "eventType": "touchEnd",
  "touchPoints": []
}
```

**Multi-touch (Pinch Zoom):**
```json
{
  "type": "input_touch",
  "eventType": "touchStart",
  "touchPoints": [
    { "x": 100, "y": 200, "id": 0 },
    { "x": 200, "y": 200, "id": 1 }
  ]
}
```

## Use Cases

### Pair Browsing

Human watches AI agent work in real-time:

```bash
# Terminal 1: AI agent
AGENT_BROWSER_STREAM_PORT=9223 agent-browser open example.com
agent-browser snapshot -i --json
# AI performs actions...

# Terminal 2: Human viewer
# Open web client connected to ws://localhost:9223
# Watch viewport in browser
```

### Remote Preview

View browser output in separate UI:

```bash
# Start agent with streaming
AGENT_BROWSER_STREAM_PORT=9223 agent-browser open app.com

# Connect from web UI
# Display frames in HTML canvas
```

### Recording Frames

Capture frames for video generation:

```javascript
const fs = require('fs');
const WebSocket = require('ws');

const ws = new WebSocket('ws://localhost:9223');
const frames = [];

ws.on('message', (data) => {
  const msg = JSON.parse(data);
  if (msg.type === 'frame') {
    frames.push(msg.data);
    
    // Save frame
    const buffer = Buffer.from(msg.data, 'base64');
    fs.writeFileSync(`frame-${frames.length}.jpg`, buffer);
  }
});
```

### Mobile Testing

Inject touch events for mobile emulation:

```javascript
// Simulate swipe
ws.send(JSON.stringify({
  type: 'input_touch',
  eventType: 'touchStart',
  touchPoints: [{ x: 100, y: 300 }]
}));

// Move touch
for (let i = 0; i < 10; i++) {
  ws.send(JSON.stringify({
    type: 'input_touch',
    eventType: 'touchMove',
    touchPoints: [{ x: 100, y: 300 - i * 20 }]
  }));
}

// End touch
ws.send(JSON.stringify({
  type: 'input_touch',
  eventType: 'touchEnd',
  touchPoints: []
}));
```

## Programmatic API

For advanced use, control via TypeScript API:

```typescript
import { BrowserManager } from 'agent-browser';

const browser = new BrowserManager();
await browser.launch({ headless: true });
await browser.navigate('https://example.com');

// Start screencast
await browser.startScreencast((frame) => {
  console.log('Frame size:', frame.metadata.deviceWidth, 'x', frame.metadata.deviceHeight);
  
  // frame.data is base64-encoded image
  const buffer = Buffer.from(frame.data, 'base64');
  // Save or process frame
}, {
  format: 'jpeg',  // or 'png'
  quality: 80,     // 0-100 (jpeg only)
  maxWidth: 1280,
  maxHeight: 720,
  everyNthFrame: 1
});

// Inject mouse click
await browser.injectMouseEvent({
  type: 'mousePressed',
  x: 100,
  y: 200,
  button: 'left',
  clickCount: 1
});

await browser.injectMouseEvent({
  type: 'mouseReleased',
  x: 100,
  y: 200,
  button: 'left'
});

// Inject keyboard
await browser.injectKeyboardEvent({
  type: 'keyDown',
  key: 'Enter',
  code: 'Enter'
});

await browser.injectKeyboardEvent({
  type: 'keyUp',
  key: 'Enter',
  code: 'Enter'
});

// Inject touch
await browser.injectTouchEvent({
  type: 'touchStart',
  touchPoints: [{ x: 100, y: 200 }]
});

await browser.injectTouchEvent({
  type: 'touchEnd',
  touchPoints: []
});

// Check status
console.log('Screencasting:', browser.isScreencasting());

// Stop screencast
await browser.stopScreencast();
```

## Performance Considerations

### Frame Rate

Control frame rate with `everyNthFrame`:

```typescript
await browser.startScreencast(callback, {
  everyNthFrame: 2  // Every 2nd frame (30fps → 15fps)
});
```

### Image Quality

Balance quality vs bandwidth:

```typescript
await browser.startScreencast(callback, {
  format: 'jpeg',
  quality: 60,     // Lower = smaller files
  maxWidth: 1024,  // Reduce resolution
  maxHeight: 768
});
```

### Network Usage

Approximate bandwidth:

- 1280x720 @ 80% quality: ~50-100KB per frame
- 30fps: 1.5-3 MB/s
- 15fps: 750KB-1.5 MB/s

## Security

The stream server:
- Rejects cross-origin connections
- Only accepts localhost connections
- No authentication (localhost only)

**Do NOT expose to public network:**
```bash
# Safe: localhost only
AGENT_BROWSER_STREAM_PORT=9223 agent-browser open example.com

# Unsafe: exposing to network
# (Don't do this without proper authentication)
```

## Troubleshooting

### Connection Refused

**Cause:** Stream port not set

**Solution:**
```bash
AGENT_BROWSER_STREAM_PORT=9223 agent-browser open example.com
```

### No Frames Received

**Cause:** Screencast not started

**Solution:** Frames start automatically when streaming enabled.

Check status message:
```json
{
  "type": "status",
  "screencasting": true  // Should be true
}
```

### Input Events Not Working

**Cause:** Incorrect event format

**Solution:** Verify JSON structure matches protocol:
```json
{
  "type": "input_mouse",
  "eventType": "mousePressed",
  "x": 100,
  "y": 200,
  "button": "left",
  "clickCount": 1
}
```

### High CPU Usage

**Cause:** Too many frames

**Solution:** Reduce frame rate:
```typescript
await browser.startScreencast(callback, {
  everyNthFrame: 3,  // Reduce to 10fps
  quality: 70        // Lower quality
});
```

## Example: Web Viewer

Simple HTML viewer:

```html
<!DOCTYPE html>
<html>
<head>
  <title>Browser Stream Viewer</title>
  <style>
    body { margin: 0; display: flex; justify-content: center; align-items: center; height: 100vh; background: #222; }
    img { max-width: 90%; max-height: 90%; border: 2px solid #444; }
  </style>
</head>
<body>
  <img id="viewer" />
  
  <script>
    const ws = new WebSocket('ws://localhost:9223');
    const img = document.getElementById('viewer');
    
    ws.onmessage = (event) => {
      const data = JSON.parse(event.data);
      if (data.type === 'frame') {
        img.src = 'data:image/jpeg;base64,' + data.data;
      }
    };
    
    // Send mouse clicks to browser
    img.onclick = (e) => {
      const rect = img.getBoundingClientRect();
      const x = (e.clientX - rect.left) / rect.width * data.metadata.deviceWidth;
      const y = (e.clientY - rect.top) / rect.height * data.metadata.deviceHeight;
      
      ws.send(JSON.stringify({
        type: 'input_mouse',
        eventType: 'mousePressed',
        x, y,
        button: 'left',
        clickCount: 1
      }));
      
      setTimeout(() => {
        ws.send(JSON.stringify({
          type: 'input_mouse',
          eventType: 'mouseReleased',
          x, y,
          button: 'left'
        }));
      }, 100);
    };
  </script>
</body>
</html>
```
