# /splid-expense

Extract expense information from the latest screenshot and create a Splid entry.

## Description

This command analyzes the most recent screenshot to extract:
- A descriptive title covering all purchased items
- The total amount (Gesamtsumme)

Then automatically creates a Splid expense entry using the extracted information.

## Usage

```
/splid-expense
```

## Implementation

```bash
#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#coreutils nixpkgs#findutils --command bash
set -euo pipefail

# Find the most recent screenshot
LATEST_SCREENSHOT=$(find ~/Screenshots -mtime -1 -type f | sort -r | head -1)

if [[ -z "$LATEST_SCREENSHOT" ]]; then
    echo "No recent screenshots found in ~/Screenshots"
    exit 1
fi

echo "Found latest screenshot: $LATEST_SCREENSHOT"
echo "Please analyze this screenshot to extract the expense title and total amount, then create the Splid entry."
```

## Workflow

1. Locates the most recent screenshot from `~/Screenshots`
2. Claude analyzes the screenshot to extract:
   - Expense title (brief description of all items)
   - Total amount (Gesamtsumme)
3. Creates Splid expense entry using `/home/markus/src/scripts/splid-claude.sh`
4. Confirms successful creation

## Example

For an Amazon cart with wallpaper, kitchen toys, and tablet holder totaling €49.97:
- Title: "Möbel-Tapete, Kochspielzeug, Tablet-Halterung"
- Amount: €49.97
- Creates expense where MARKUS pays for HEIKE