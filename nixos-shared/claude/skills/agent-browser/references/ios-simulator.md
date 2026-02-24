# iOS Simulator Automation

Control real Mobile Safari in the iOS Simulator (or on real devices) for authentic mobile web testing. Uses Appium with XCUITest under the hood.

## Requirements

- macOS with Xcode installed
- iOS Simulator runtimes (download via Xcode → Settings → Platforms)
- Appium with XCUITest driver

## Setup

```bash
# Install Appium globally
npm install -g appium

# Install the XCUITest driver for iOS
appium driver install xcuitest
```

## List available devices

```bash
agent-browser device list

# Output:
# Available iOS Simulators:
#
#   ○ iPhone 16 Pro (iOS 18.0)
#     F21EEC0D-7618-419F-811B-33AF27A8B2FD
#   ○ iPhone 16 Pro Max (iOS 18.0)
#     50402807-C9B8-4D37-9F13-2E00E782C744
#   ○ iPad Pro 13-inch (M4) (iOS 18.0)
#     3A6C6436-B909-4593-866D-91D1062BB070
```

## Basic usage

Use the `-p ios` flag to enable iOS mode. The workflow is identical to desktop:

```bash
# Launch Safari on iPhone 16 Pro
agent-browser -p ios --device "iPhone 16 Pro" open https://example.com

# Get snapshot with refs (same as desktop)
agent-browser -p ios snapshot -i

# Interact using refs
agent-browser -p ios tap @e1
agent-browser -p ios fill @e2 "text"

# Take screenshot
agent-browser -p ios screenshot mobile.png

# Close session (shuts down simulator)
agent-browser -p ios close
```

## Mobile-specific commands

```bash
# Swipe gestures
agent-browser -p ios swipe up
agent-browser -p ios swipe down
agent-browser -p ios swipe left
agent-browser -p ios swipe right

# Swipe with distance (pixels)
agent-browser -p ios swipe up 500

# Tap (alias for click, semantically clearer for touch)
agent-browser -p ios tap @e1
```

## Environment variables

Configure iOS mode via environment variables to avoid repeating flags:

```bash
export AGENT_BROWSER_PROVIDER=ios
export AGENT_BROWSER_IOS_DEVICE="iPhone 16 Pro"

# Now all commands use iOS automatically
agent-browser open https://example.com
agent-browser snapshot -i
agent-browser tap @e1
```

| Variable | Description |
|---|---|
| `AGENT_BROWSER_PROVIDER` | Set to `ios` to enable iOS mode |
| `AGENT_BROWSER_IOS_DEVICE` | Device name (e.g., "iPhone 16 Pro") |
| `AGENT_BROWSER_IOS_UDID` | Device UDID (alternative to device name) |

## Supported devices

All iOS Simulators available in Xcode:

- All iPhone models (iPhone 15, 16, 17, SE, etc.)
- All iPad models (iPad Pro, iPad Air, iPad mini, etc.)
- Multiple iOS versions (17.x, 18.x, etc.)

## Real device support

Appium can control Safari on real iOS devices connected via USB.

### 1. Get your device UDID

```bash
xcrun xctrace list devices
```

### 2. Sign WebDriverAgent (one-time)

```bash
cd ~/.appium/node_modules/appium-xcuitest-driver/node_modules/appium-webdriveragent
open WebDriverAgent.xcodeproj
```

In Xcode: select `WebDriverAgentRunner` target → Signing & Capabilities → set your Team.

### 3. Use with agent-browser

```bash
agent-browser -p ios --device "<DEVICE_UDID>" open https://example.com
# Or by name if unique:
agent-browser -p ios --device "John's iPhone" open https://example.com
```

**Real device notes:**
- First run installs WebDriverAgent (may show Trust prompt on device)
- Device must be unlocked and connected via USB
- Go to Settings → General → VPN & Device Management to trust the developer certificate

## Differences from desktop

| Feature | Desktop | iOS |
|---|---|---|
| Browser | Chromium/Firefox/WebKit | Safari only |
| Tabs | Supported | Single tab only |
| PDF export | Supported | Not supported |
| Screencast | Supported | Not supported |
| Swipe gestures | Not native | Native support |

## Performance

- **First launch:** 30–60 seconds to boot simulator and start Appium
- **Subsequent commands:** Fast (simulator stays running between commands)
- **Close command:** Shuts down simulator and Appium server

## Troubleshooting

### Appium not found
```bash
npm install -g appium
appium driver install xcuitest
appium --version  # verify
```

### No simulators available
Open Xcode → Settings → Platforms and download iOS Simulator runtimes.

### Simulator won't boot
Try booting manually from the Simulator app first, then retry.

### Real device: Trust prompt
On first WebDriverAgent install, go to:
Settings → General → VPN & Device Management → trust the developer certificate.
