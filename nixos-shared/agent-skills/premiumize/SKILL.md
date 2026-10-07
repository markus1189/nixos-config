---
name: premiumize
description: "Manage downloads and cloud storage on Premiumize.me. Add downloads from URLs, magnets, or NZB files, monitor transfer progress, get direct download/streaming links, browse and search cloud files, check cache availability, and manage folders. Use when the user mentions Premiumize, premium downloads, debrid service, cloud torrents, magnet links, direct download links, or wants to manage their Premiumize.me account."
---

# Premiumize.me Download Manager

Manage downloads, transfers, and cloud storage via the Premiumize.me API.

**API key:** stored in `pass api/premiumize` (get from https://www.premiumize.me/account); `PREMIUMIZE_API_KEY` overrides it.

**Default folder:** the root folder named `AgentSkill` (`DEFAULT_FOLDER_NAME` in the script) — all transfers land here unless a folder_id is explicitly provided.

## Core Workflows

### 1. Add a Download (URL, Magnet, NZB)

```bash
# From URL or magnet link
./scripts/premiumize-api.sh transfer-create "magnet:?xt=urn:btih:..." [folder_id]
./scripts/premiumize-api.sh transfer-create "https://example.com/file.zip" [folder_id]

# From NZB/DLC file
./scripts/premiumize-api.sh transfer-create-file "/path/to/file.nzb" [folder_id]
```

### 2. Check if Cached (Instant Download)

Always check cache first — cached items can be downloaded immediately via `directdl`:

```bash
# Check availability
./scripts/premiumize-api.sh cache-check-pretty "magnet:?xt=..." "https://..."

# If cached → get instant links
./scripts/premiumize-api.sh directdl-pretty "magnet:?xt=..."
```

`directdl` returns an array of `content` objects with `link`, `stream_link`, `path`, and `size`.

### 3. Monitor Transfers

```bash
# List active/pending transfers
./scripts/premiumize-api.sh transfers-pretty

# Raw JSON for processing
./scripts/premiumize-api.sh transfers
```

Transfer statuses: `waiting`, `queued`, `running`, `seeding`, `finished`, `error`, `timeout`, `deleted`, `banned`

### 4. Browse & Search Files

```bash
# Browse root folder
./scripts/premiumize-api.sh folder-list-pretty

# Browse specific folder
./scripts/premiumize-api.sh folder-list-pretty "FOLDER_ID"

# Search by name
./scripts/premiumize-api.sh folder-search-pretty "movie name"

# Get file details with download/stream links
./scripts/premiumize-api.sh item-details-pretty "ITEM_ID"

# List ALL files (flat, no folder structure)
./scripts/premiumize-api.sh item-listall
```

### 5. Get Download & Stream Links

```bash
# From item ID (file already in cloud storage)
./scripts/premiumize-api.sh item-details "ITEM_ID" | jq -r '.link'
./scripts/premiumize-api.sh item-details "ITEM_ID" | jq -r '.stream_link'

# From URL/magnet (cached content, instant)
./scripts/premiumize-api.sh directdl "magnet:?xt=..." | jq -r '.content[].link'

# Download file to local disk
./scripts/premiumize-api.sh download "ITEM_ID" [output_path]
```

### 6. Clean Up

```bash
# Delete specific transfer
./scripts/premiumize-api.sh transfer-delete "TRANSFER_ID"

# Clear all finished transfers
./scripts/premiumize-api.sh transfer-clear

# Delete a file or folder
./scripts/premiumize-api.sh item-delete "ITEM_ID"
./scripts/premiumize-api.sh folder-delete "FOLDER_ID"
```

### 7. File Management

```bash
# Create folder
./scripts/premiumize-api.sh folder-create "New Folder" [parent_id]

# Move files/folders
./scripts/premiumize-api.sh folder-paste "TARGET_FOLDER_ID" --files id1 id2 --folders id3

# Rename
./scripts/premiumize-api.sh item-rename "ITEM_ID" "new-name.mkv"
./scripts/premiumize-api.sh folder-rename "FOLDER_ID" "New Name"

# Generate zip for download
./scripts/premiumize-api.sh zip-generate --files id1 id2 --folders id3
```

### 8. Account & Services

```bash
# Account status (premium expiry, fair use, storage)
./scripts/premiumize-api.sh account-pretty

# List supported download services/hosters
./scripts/premiumize-api.sh services
```

**Fair use:** `account-pretty` shows the share of the fair-use limit already used and the booster points left. Before adding anything large, check its size with `cache-check-pretty` and the current usage, and ask the user if it would eat a noticeable part of the remainder.

## Recommended Workflow: Add & Get Link

The most common workflow — add something and get the download link:

```bash
# 1. Check if cached
./scripts/premiumize-api.sh cache-check "magnet:?xt=..."

# 2a. If cached → instant direct download link
./scripts/premiumize-api.sh directdl-pretty "magnet:?xt=..."

# 2b. If NOT cached → create transfer, wait for completion
./scripts/premiumize-api.sh transfer-create "magnet:?xt=..."
# ... poll with transfers-pretty until finished ...
./scripts/premiumize-api.sh transfers-pretty

# 3. Browse result and get links
./scripts/premiumize-api.sh folder-list-pretty
./scripts/premiumize-api.sh item-details-pretty "ITEM_ID"
```

## Commands

Run `./scripts/premiumize-api.sh` without arguments for the full command list. Commands ending in `-pretty` produce human-readable output; without the suffix they return raw JSON for `jq` processing. API errors exit non-zero with the message on stderr.

Confirm with the user before any `*-delete` or `transfer-clear`.

**Script Execution:** Scripts should be executed from the skill directory.
All scripts use Nix shebangs so no manual dependency installation is required.
