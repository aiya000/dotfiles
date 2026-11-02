#!/usr/bin/env bash
#
# open-in-main-window.sh
#
# Open the given URL in a new tab of the "main" Google Chrome window.
#
# The "main" window is identified by its FIRST tab's URL starting with
# MAIN_WINDOW_MARKER_PREFIX. The marker is:
#   https://www.google.com/search?q=メインウィンドウ
# (URL-encoded, since Chrome returns the encoded form).
#
# If no main window is found, the URL is opened in a new Chrome window.
#
# Usage:
#   open-in-main-window.sh <url>

set -euo pipefail

# The prefix that identifies the main window's first tab.
# Chrome appends extra query params (&sca_esv=... etc.), so we match by prefix.
MAIN_WINDOW_MARKER_PREFIX="https://www.google.com/search?q=%E3%83%A1%E3%82%A4%E3%83%B3%E3%82%A6%E3%82%A3%E3%83%B3%E3%83%89%E3%82%A6"

if [[ $# -lt 1 || -z "${1:-}" ]]; then
  echo "usage: $(basename "$0") <url>" >&2
  exit 1
fi

url="$1"

# TEMP DEBUG: always record each invocation (removed once routing is confirmed).
printf '%s\t%s\n' "$(date '+%Y-%m-%d %H:%M:%S')" "$url" >>"$HOME/oimw-debug.log"


# Pass values to AppleScript as positional arguments ("argv") to avoid any
# string-interpolation / injection issues with the URL or the marker.
osascript - "$url" "$MAIN_WINDOW_MARKER_PREFIX" <<'APPLESCRIPT'
on run argv
    set theURL to item 1 of argv
    set markerPrefix to item 2 of argv

    tell application "Google Chrome"
        set targetWindow to missing value

        -- Find the main window: the one whose first tab's URL starts with markerPrefix.
        repeat with w in windows
            set firstTabURL to ""
            try
                set firstTabURL to URL of (tab 1 of w)
            end try
            if firstTabURL starts with markerPrefix then
                set targetWindow to w
                exit repeat
            end if
        end repeat

        if targetWindow is missing value then
            -- No main window found: open in a new window as a fallback.
            make new window
            set targetWindow to window 1
            set URL of (active tab of targetWindow) to theURL
        else
            -- Open the URL in a new tab of the main window.
            tell targetWindow
                make new tab with properties {URL:theURL}
                set active tab index to (count of tabs)
            end tell
            set index of targetWindow to 1
        end if
    end tell

    -- Bring Chrome to the foreground.
    tell application "Google Chrome" to activate
end run
APPLESCRIPT
