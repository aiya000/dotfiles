---
name: vrchat-force-purge-all-favorite-worlds
description: Force-delete every favorited world on the authenticated VRChat account via VRChat's REST API. Use when the user asks to purge, clear, wipe, or delete all their VRChat favorite worlds.
allowed-tools: Bash(curl:*), Bash(jq:*), Bash(mkdir:*), Bash(kill:*), Bash(ps:*), Bash(touch:*), Bash(rm-dust:*), Bash(sleep:*)
---

# vrchat-force-purge-all-favorite-worlds

Deletes ALL favorited worlds (all groups: `worlds1`..`worldsN`) from the authenticated
VRChat account. This is destructive and irreversible via the API — favorites are not
recoverable after deletion.

**Fixed paths used by this skill (intentionally NOT the per-session scratchpad — see Notes):**

- `WORKDIR=/tmp/vrchat-force-purge-all-favorite-worlds`
- `COOKIE_JAR=$WORKDIR/cookies.jar`
- `FAVORITES_JSON=$WORKDIR/favorites.json`
- `LOG=$WORKDIR/delete.log`
- `LOCKFILE=/tmp/vrchat-force-purge-all-favorite-worlds.lock` (sibling of `WORKDIR`, not inside it)

## Behavior

0. **Credential precondition check** — before anything else:
    - Verify both `$VRC_USER` and `$VRC_PASS` are set and non-empty
    - If either is missing: stop immediately. Tell the user to run
      `export VRC_USER=... VRC_PASS=...` (optionally `VRC_CONTACT=...`, see Notes) in their shell,
      then re-invoke this skill. Do not attempt login with a partial/empty credential

1. **Concurrency guard (lock)**:
    - `mkdir -p "$WORKDIR"`
    - If `$LOCKFILE` exists: read the PID from it, check liveness with `kill -0 <PID>`
        - Alive → stop, report another purge is already running (show PID + lock timestamp), refuse
        - Dead (stale) → note it, `rm-dust -f "$LOCKFILE"`, continue
    - Do not write the lock yet — it's written atomically with the delete loop in step 8

2. **Authenticate**:
    - `rm-dust -f "$COOKIE_JAR"` first (no stale cookies)
    - `UA="vrchat-force-purge-all-favorite-worlds/1.0 (${VRC_CONTACT:-contact-not-provided})"`
    - Call **`api.vrchat.cloud` directly** (never `api.vrchat.com` — it 307-redirects cross-host
      and curl drops the `Authorization` header on that redirect):
      `curl -sS -u "$VRC_USER:$VRC_PASS" -A "$UA" -c "$COOKIE_JAR" -b "$COOKIE_JAR" -o "$WORKDIR/auth.json" -w '%{http_code}' https://api.vrchat.cloud/api/1/auth/user`
    - Non-2xx → stop, report likely-bad credentials, no blind retry
    - If `auth.json` contains `requiresTwoFactorAuth` → step 3, else → step 4

3. **Two-factor auth (only if required)**:
    - Prefer `totp` if listed, else `otp` (email)
    - `AskUserQuestion` free-text for the code
    - `POST /api/1/auth/twofactorauth/{totp|otp}/verify` with `{"code": "..."}`, same cookie jar
    - Check for `"verified": true`; on failure allow one retry (or offer switching totp/otp), then
      stop and report if it fails twice
    - Never print the cookie jar's contents to chat

4. **List all favorited worlds (paginate)**:
    - `GET /api/1/favorites?type=world&n=100&offset=N`, loop until a page returns <100
    - Accumulate all pages into `$FAVORITES_JSON`
    - Per entry: `id` (`fvrt_...`) = favorite entry ID, **used for deletion**; `favoriteId`
      (`wrld_...`) = the target world's ID, used only to look up its name; `tags[0]` = group name

5. **Backup before deletion (required — do not skip, do not proceed without a verified backup)**:
    - Use `AskUserQuestion` (free-text) to ask the user for a backup destination: a directory or
      file path where the full favorites list should be saved before anything is deleted
    - Resolve the answer:
        - If it names an existing directory (or ends with `/`): write to
          `<path>/vrchat-favorites-backup-<UTC-timestamp>.jsonl`
        - Otherwise: treat it as a literal file path (`mkdir -p` any missing parent directory first)
    - Write one JSON object per line (JSONL) for **every** entry in `$FAVORITES_JSON` — not just the
      preview sample — e.g. `jq -c '.[]' "$FAVORITES_JSON" > "$BACKUP_PATH"`
    - Verify: the backup file's line count equals the total favorites count from step 4. If it
      doesn't match, or the write failed, **stop** — do not proceed to preview/deletion without a
      verified backup
    - Report the resolved backup path and line count to the user as chat text
    - If the resolved path is inside a git-tracked directory, mention this to the user (their
      favorites list would sit there as a plaintext file that could get committed) — informational
      only, not a hard stop

6. **Preview before deleting (write as your own chat text, not just tool output)**:
    - Group by `tags[0]`; for each group's first 10 entries, `GET /api/1/worlds/{favoriteId}` to
      fetch each world's `name`
    - In the chat reply: list group name, count, up to 10 world names ("...and N more" if truncated),
      and the grand total across all groups

7. **Final confirmation**:
    - `AskUserQuestion` with the total count `N` stated explicitly: "Delete all N favorite worlds"
      vs "Cancel". Only proceed on explicit delete confirmation

8. **Deletion loop** (single Bash invocation, so the lock's PID stays valid for the whole loop):
    - `touch "$LOG"`
    - `echo "PID=$$" > "$LOCKFILE"; echo "STARTED=$(date -Is)" >> "$LOCKFILE"`
    - `trap 'rm-dust -f "$LOCKFILE"' EXIT`
    - For each entry (by `id`): `curl -sS -A "$UA" -c "$COOKIE_JAR" -b "$COOKIE_JAR" -o /dev/null -w '%{http_code}' -X DELETE "https://api.vrchat.cloud/api/1/favorites/$ID"`
    - Log each attempt: `<index>/<total> id=<id> tag=<tag> status=<code> ts=<iso8601>` to `$LOG`
    - Default ~0.2-0.3s sleep between requests (397 deletes at this pace previously hit zero `429`s)
    - **Reactive 429 handling only**: on `429`, honor `Retry-After` if present (else 5-10s fixed),
      retry the same entry, up to 3 attempts before marking it failed and moving on
    - Any other non-2xx: log as failed, continue to next entry (don't abort the whole run)
    - Every ~20 iterations, `touch "$LOCKFILE"` as a heartbeat

9. **Post-run verification**:
    - Re-fetch favorites (`offset=0`), check length
    - Report chat-text summary: attempted / succeeded / failed (with IDs + last status), and
      explicit confirmation of final list state. Remind the user where the pre-deletion backup was
      saved (path from step 5)

10. **Cleanup (always)**:
    - `rm-dust -f "$COOKIE_JAR" "$FAVORITES_JSON"`
    - Lock releases via the step-8 `trap`; if the run stopped before step 8, there was nothing to release
    - `$LOG` may remain in `$WORKDIR` for post-run inspection (contains only index/id/status/timestamp)
    - The backup file written in step 5 is **never** deleted by this skill

## Environment Variables

- `VRC_USER` (required), `VRC_PASS` (required) — exported by the user before invoking this skill
- `VRC_CONTACT` (optional, recommended) — contact string for the `User-Agent` header; if unset, a
  placeholder is used and the skill should warn requests may be rejected without it

## Secret Scan

Before printing any command output/summary to chat, and before this file is ever edited/committed:

- API keys/tokens (`ghp_`, `gho_`, `AKIA`, `sk-`, `xox`, `-----BEGIN.*PRIVATE KEY`)
- Sensitive-named vars holding values (`API_KEY=`, `_SECRET=`, `_TOKEN=`, `PASSWORD=`), or a literal
  VRChat password/cookie value
- Hardcoded absolute home paths (prefer `~`)
- Personal/org proper nouns (e.g. do not hardcode a real contact email into this file — use
  `$VRC_CONTACT` at runtime instead)

If found, use `AskUserQuestion`: "Continue as-is" / "Fix then continue" / "Cancel". Proceed only on
"Continue as-is" or if nothing suspicious was found.

## Does Not

1. Does not accept credentials as skill arguments or from any file — only `$VRC_USER`/`$VRC_PASS`
2. Does not retry a failed 2FA verification more than once before stopping
3. Does not preemptively throttle beyond ~0.2-0.3s between deletes unless a `429` actually occurs
4. Does not delete anything without explicit "Delete all N" confirmation via `AskUserQuestion`
5. Does not run if another instance's lock file shows a live PID — refuses instead of double-processing
6. Does not persist the cookie jar/credentials/2FA code in the git-tracked dotfiles tree, print the
   cookie jar's contents to chat, or leave the cookie jar behind after the run
7. Does not proceed to preview/confirmation/deletion without first writing and verifying a full
   JSONL backup of the favorites list at a user-specified path (step 5)
8. Does not delete the user's backup file itself — only its own working files (`$COOKIE_JAR`,
   `$FAVORITES_JSON`, lock file) are cleaned up

## Notes

- **Sandbox caveat**: `api.vrchat.cloud` is not in the default Bash sandbox network allowlist, and
  the fixed `$WORKDIR`/`$LOCKFILE` paths (`/tmp/vrchat-force-purge-all-favorite-worlds*`) are
  outside the sandbox filesystem write allowlist too — as is, likely, whatever path the user picks
  for the step 5 backup. In practice this means every `curl`/`mkdir`/`touch`/`rm-dust`/backup-write
  call in this skill runs with `dangerouslyDisableSandbox`. This is not new overhead beyond what the
  network restriction already requires — once a Bash call needs the sandbox disabled for the
  network hop, doing the file I/O in that same call costs nothing extra
- Always target `api.vrchat.cloud` directly, never `api.vrchat.com`
- The lock file is intentionally at a **fixed, cross-session** path, not the per-session scratchpad
  ($TMPDIR) — this is so a second, independently-launched Claude Code session running this same
  skill concurrently can see and respect it (a prior manual run without this guard led to two
  sessions unknowingly deleting from the same list at once, which looked like a stall/rate-limit
  from either session's point of view)
- Reruns are naturally resumable — step 4 always re-fetches the live favorites list, so no separate
  resume bookkeeping is needed
- Replaces the earlier `./us`/`./pw` plaintext-file convention entirely — do not reintroduce
  file-based credentials
- Use `rm-dust` for cleanup, never `rm` (denied globally in `settings.json`)
