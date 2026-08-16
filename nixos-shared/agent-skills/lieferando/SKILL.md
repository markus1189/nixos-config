---
name: lieferando
description: "Lists restaurants delivering (or offering pickup) to a location via the Lieferando / Just Eat Takeaway discovery API, with ratings, ETA, distance, minimum order value, delivery fee and current deals. Use when the user asks about ordering food, food delivery, takeaway, what restaurants deliver to an address, where to get döner/pizza/sushi/burgers, delivery fees or minimum orders, or mentions Lieferando, Just Eat, Takeaway.com or lieferando.de/lieferando.at. Covers Germany and Austria only."
---

# Lieferando

`./scripts/lieferando.sh --lat FLOAT --lon FLOAT [QUERY] [options]` queries the public Just Eat
Takeaway discovery API. No authentication, no API key. Run `--help` for the full flag list.

**`--lat` and `--lon` are required — there is no default location.** The script takes raw
coordinates, not addresses. Ask the user where they want to order from, then geocode it with the
**google-maps** skill (`./scripts/maps-api.sh geocode-pretty "<address>"`) and pass the result
through. Do not guess coordinates.

```bash
./scripts/lieferando.sh --lat 50.1109 --lon 8.6821 döner --open      # open now, best-rated first
./scripts/lieferando.sh --lat 50.1109 --lon 8.6821 pizza --sort min --json
./scripts/lieferando.sh --lat 50.1109 --lon 8.6821 --collection --sort distance   # pickup, nearest
```

Defaults otherwise: delivery, sorted by rating, `--country de`.

## Non-obvious behaviour

- **QUERY is filtered locally, not by the API.** The endpoint's own `textSearch` parameter is
  ignored server-side, so the script fetches every restaurant for the coordinates and matches
  QUERY against name + cuisine tags in jq. Matching is case- and umlaut-insensitive
  (`doner` matches `Döner`), and a substring — so a query is cheap, but a *typo* silently
  returns "No matches." rather than an error.
- **`--country` must match the coordinates.** German lat/lon under `--country at` returns an
  empty restaurant list, not an error. The script says so on stderr; don't blame the QUERY.
- **ETA is meaningless while a restaurant is closed**, so the table prints the opening time
  instead (`opens 11:00`). Closed places are included by default — pass `--open` to drop them.
- **FEE is the cheapest delivery band.** Some restaurants charge less above a higher order
  value, so the single number hides that tiering.
- Exit 2 means network/API failure (endpoint moved, coordinates rejected); exit 1 is a usage
  error, including missing `--lat`/`--lon`. Zero matches is a *success* (exit 0, `[]` under
  `--json`).

**Script Execution:** Run scripts from the skill directory. The Nix shebang pulls its own
dependencies (curl, jq, util-linux) — no installation needed.
