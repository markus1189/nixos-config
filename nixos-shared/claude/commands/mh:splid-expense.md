---
description: Create a Splid expense (title + Gesamtsumme) from the latest receipt/cart screenshot in ~/Stuff/Today
argument-hint: [expense|refund|split]
---

# /splid-expense

Extract expense information from the latest screenshot and create a Splid entry.

## Description

This command analyzes the most recent screenshot to extract:
- A descriptive title covering all purchased items
- The total amount (Gesamtsumme)

Then automatically creates a Splid expense entry using the extracted information.

## Usage

```
/splid-expense [TYPE]
```

**Arguments:**
- `TYPE` (optional): Expense type - determines who pays whom
  - `expense` - Primary user pays for secondary user (default if omitted)
  - `refund` - Secondary user pays for primary user
  - `split` - 50/50 split between both
  - `50/50 for both` - Alternative way to specify split

**Examples:**
```
/splid-expense                # Default: expense type
/splid-expense split          # 50/50 split
/splid-expense 50/50 for both # 50/50 split (natural language)
/splid-expense refund         # Refund type
```

## Implementation

```bash
# Newest image by mtime (Today is a symlink; -t sorts by age, not name)
LATEST_SCREENSHOT=$(ls -t ~/Stuff/Today/*.png ~/Stuff/Today/*.jpg ~/Stuff/Today/*.jpeg 2>/dev/null | head -1)
[[ -n "$LATEST_SCREENSHOT" ]] || { echo "No screenshots found in ~/Stuff/Today"; exit 1; }
echo "$LATEST_SCREENSHOT"
```

## Workflow

1. Locate the newest screenshot in `~/Stuff/Today` (snippet above) and read it
2. Extract:
   - Expense title (brief description of all items)
   - Total amount (Gesamtsumme)
3. Duplicate check: `/home/markus/src/scripts/splid-claude.sh list 7` — if an
   entry with the same amount and a similar title exists, stop and ask
4. Create the entry:
   `/home/markus/src/scripts/splid-claude.sh create "TITLE" "AMOUNT" TYPE`
   (TYPE: `expense` | `refund` | `split`; AMOUNT accepts `49.97` or `49,97 €`).
   Pass the bare title: the script itself prepends `50/50: ` (split) and
   `Gutschrift: ` (refund)
5. Confirm successful creation

## Examples

**Default expense:**
```
/splid-expense
```
For an Amazon cart with wallpaper, kitchen toys, and tablet holder totaling €49.97:
- Title: "Möbel-Tapete, Kochspielzeug, Tablet-Halterung"
- Amount: €49.97
- Creates: Primary user pays €49.97 for secondary user

**50/50 split:**
```
/splid-expense split
/splid-expense 50/50 for both
```
For an anti-slip carpet runner totaling €23.99:
- Title: "50/50: Antirutsch Teppich Läufer"
- Amount: €23.99
- Creates: Primary user pays €23.99, split equally between both users

**Refund:**
```
/splid-expense refund
```
- Title: "Gutschrift: [extracted title]"
- Creates: Secondary user pays for primary user