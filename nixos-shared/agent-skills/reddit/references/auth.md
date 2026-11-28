# Reddit OAuth setup

Only for (re-)linking the account. Normal use needs none of it.

## Linking

```bash
./scripts/reddit_auth.py
```

Prints an authorize URL, catches the callback locally, prints a permanent refresh
token to put into `secrets/reddit.env.age` (`agenix -e`, then rebuild). Scopes: `identity read mysubreddits
history`. Interactive browser login — have the **user** run it (`! <cmd>`); it
cannot be driven from a tool call.

`/run/agenix/reddit.env` holds `REDDIT_CLIENT_ID` / `REDDIT_CLIENT_SECRET` (the
app) and `REDDIT_REFRESH_TOKEN` (user context); same-named env vars override it.

## Traps

**`invalid_grant`** — the password grant works only for apps of type "script" and
breaks under 2FA. Username/password credentials are dead weight here; they return this. Use the flow above.

**`invalid redirect_uri parameter`** — Reddit byte-compares the redirect URI
against the registration at <https://www.reddit.com/prefs/apps>, trailing slash
included. This app is registered as `http://localhost:8000`, the script's default.
If that changes, pass `--redirect-uri`; the callback server binds to its host:port.
