---
name: heike-amazon
description: "Lists Heike's unread Amazon share mails, checks a basket screenshot against them, and books the total in Splid. Invoked only by the user via /heike-amazon."
disable-model-invocation: true
---

# heike-amazon

Both scripts read `/run/agenix/splid.env` (mail filter, Splid IDs and Parse
headers). Run them from this skill's directory.

## 1. List the links

```bash
./scripts/fetch.py
```

One JSON line per unread inbox mail matching the filter: `date`, `title`,
`link`, `note`. Show a table (date, item, link, note); mark notes 🟠: they
carry quantity, size and colour wishes. A line with `error` has no Amazon link:
show its body.

## 2. Check the basket

The user pastes a basket screenshot. Per table row compare item, quantity and
any size/colour from the note; flag missing and extra items. Sum line prices
(unit price × quantity) and have the user confirm the total.

## 3. Book in Splid

Title: `Amazon (<short item list>)`, a few German words per item with `2×` for
quantities, so the entry is recognisable later, e.g.
`Amazon (Herbstdeko, Hausschuhe, 2× Kalender, Holzringe, Sticker)`.

Default: Markus paid, for Heike. Both owing 50/50 is rare; only on request.

```bash
./scripts/splid.py --title "…" --amount 63.94 --payer markus --for heike --dry-run
```

Show the dry-run, then run without `--dry-run` only after explicit yes: the
entry is immediately visible to Heike.

`--for heike markus` writes shares of 0.5 each, a format not yet confirmed
against the Splid app: after the first such entry, have the user check the
split in the app.

## 4. Remind

gws access is read-only: tell the user to mark the processed mails read in
Gmail, or step 1 lists them again next time.
