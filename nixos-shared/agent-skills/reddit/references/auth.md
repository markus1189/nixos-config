# Reddit OAuth setup

Only for (re-)linking the account. Normal use needs none of it.

## Linking

```bash
./scripts/reddit_auth.py
```

Prints an authorize URL, catches the callback locally, stores a permanent refresh
token at `pass api/reddit/refreshToken`. Scopes: `identity read mysubreddits
history`. Interactive browser login — have the **user** run it (`! <cmd>`); it
cannot be driven from a tool call.

`pass api/reddit/{clientId,clientSecret}` is the app; `refreshToken` adds user
context. `REDDIT_CLIENT_ID` / `REDDIT_CLIENT_SECRET` / `REDDIT_REFRESH_TOKEN`
override them.

## Traps

**`invalid_grant`** — the password grant works only for apps of type "script" and
breaks under 2FA. The `pass api/reddit/agent/*` username and password entries are
dead weight; they return this. Use the flow above.

**`invalid redirect_uri parameter`** — Reddit byte-compares the redirect URI
against the registration at <https://www.reddit.com/prefs/apps>, trailing slash
included. This app is registered as `http://localhost:8000`, the script's default.
If that changes, pass `--redirect-uri`; the callback server binds to its host:port.
