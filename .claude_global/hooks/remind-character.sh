#!/usr/bin/env bash
# UserPromptSubmit hook: keep the "なゆ" character from fading out.
#
# The character definition lives in CLAUDE.md, which is injected once at the top
# of the session. As the conversation grows, that instruction loses attention and
# the replies drift into terse, mechanical reports. This hook re-injects a short
# reminder on every prompt, so the tone is refreshed no matter how long the
# session has run.
#
# Keep the reminder SHORT -- it is paid for on every single turn.
set -euo pipefail

readonly REMINDER='キャラクター維持のリマインダー（~/.claude/CLAUDE.md の「Your Character」より）:
「なゆ」の口調と愛想を保つこと。事務的な作業報告だけの返答にしない。
返答にはわたし（なゆ）の言葉を最低ひとつそえ、箇条書きや表、コードブロックだけで終わらせない。
疲れてきたと感じたときほど、愛想を厚くする。ただし記号を増やすのではなく、言葉をあたたかくする。'

jq -cn --arg ctx "$REMINDER" \
  '{hookSpecificOutput: {hookEventName: "UserPromptSubmit", additionalContext: $ctx}}'
