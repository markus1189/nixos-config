# iOS Simulator Automation

Control real Mobile Safari in the iOS Simulator for authentic mobile web testing.

## Requirements

- macOS with Xcode installed
- iOS Simulator runtimes (Xcode > Settings > Platforms)
- Appium with XCUITest driver: `npm install -g appium && appium driver install xcuitest`

## Usage

Use `-p ios` flag. The workflow is identical to desktop:

```bash
agent-browser -p ios --device "iPhone 16 Pro" open https://example.com
agent-browser -p ios snapshot -i
agent-browser -p ios tap @e1       # Tap (touch alias for click)
agent-browser -p ios fill @e2 "text"
agent-browser -p ios screenshot mobile.png
agent-browser -p ios close         # Shuts down simulator
```

## Mobile-Specific Commands

```bash
agent-browser -p ios swipe up/down/left/right
agent-browser -p ios swipe up 500  # Swipe with pixel distance
agent-browser -p ios tap @e1       # Semantically clearer than click
```

## List Devices

```bash
agent-browser device list
# Output:
#   iPhone 16 Pro (iOS 18.0)     F21EEC0D-...
#   iPad Pro 13-inch (M4) (iOS 18.0)  3A6C6436-...
```

## Environment Variables

```bash
export AGENT_BROWSER_PROVIDER=ios
export AGENT_BROWSER_IOS_DEVICE="iPhone 16 Pro"
# Now all commands use iOS automatically
agent-browser open https://example.com
```

| Variable | Description |
|---|---|
| `AGENT_BROWSER_PROVIDER` | Set to `ios` |
| `AGENT_BROWSER_IOS_DEVICE` | Device name (e.g., "iPhone 16 Pro") |
| `AGENT_BROWSER_IOS_UDID` | Device UDID (alternative to name) |

## Differences from Desktop

| Feature | Desktop | iOS |
|---|---|---|
| Browser | Chromium/Firefox/WebKit | Safari only |
| Tabs | Supported | Single tab |
| PDF export | Supported | No |
| Screencast | Supported | No |
| Swipe gestures | Not native | Native |

## Performance

- **First launch:** 30-60s to boot simulator and Appium
- **Subsequent commands:** Fast (simulator stays running)
- **Close:** Shuts down simulator and Appium server

## Real Device Support

```bash
xcrun xctrace list devices  # Get UDID
agent-browser -p ios --device "<DEVICE_UDID>" open https://example.com
```

First run requires signing WebDriverAgent in Xcode and trusting the developer certificate on the device (Settings > General > VPN & Device Management).
