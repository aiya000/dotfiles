#!/usr/bin/env bash
# SessionStart hook (matcher: "startup|clear")
#
# Notices that the previous session ended with create-handoff, and has Claude
# ask whether to pick that handoff up -- instead of either ignoring it or
# reading it unasked.
#
# Why here and not in the read-handoff skill: read-handoff *is* the order
# "read the handoff". By the time it runs, the answer is already yes, so
# asking there would only be in the way. The one moment the answer is still
# unknown is the start of a session, which is this hook.
#
# Only a handoff written for *this* project is offered. read-handoff's
# fallback to another project's newest file is right when a human asked for
# it, and noise when nobody did.
#
# A "no" is remembered per handoff: declining leaves an empty `<name>.declined`
# next to `<name>.md`, and that handoff is not offered again. Only a decline
# marks it -- an offer the user skipped past stays on the table. The marker
# does not end in `.md`, so read-handoff can still read the file when asked.
# The decline-handoff-suggestion skill writes the same marker; keep the two in
# step.
#
# The injected text is written as factual statements rather than imperatives on
# purpose. Imperative, out-of-band-sounding instructions trip Claude's
# prompt-injection defenses, which makes it surface the text to the user
# instead of acting on it.
#   https://code.claude.com/docs/en/hooks#add-context-for-claude
set -euo pipefail

input=$(cat)

cwd=''
if command -v jq >/dev/null 2>&1; then
  cwd=$(printf '%s' "$input" | jq -r '.cwd // empty')
fi
[ -n "$cwd" ] || cwd=$PWD
cd "$cwd" 2>/dev/null || exit 0

# The same directory order create-handoff writes in.
if [ -d "$HOME/tmp" ]; then
  dir="$HOME/tmp/claude-handoff"
else
  dir="${TMPDIR:-/tmp}/claude-handoff"
fi
[ -d "$dir" ] || exit 0

# The same project name create-handoff derives: the repository, not the
# current directory, so a git worktree called `develop` is still reported
# under the project it belongs to.
common=$(git rev-parse --path-format=absolute --git-common-dir 2>/dev/null || true)
if [ -n "$common" ]; then
  project=$(basename "$(dirname "$common")")
else
  project=$(basename "$PWD")
fi

# The file names carry the timestamp, so the last one by name is the newest.
newest=''
for f in "$dir/$project"-*.md; do
  [ -e "$f" ] || continue
  newest=$f
done
[ -n "$newest" ] || exit 0

declined="${newest%.md}.declined"
[ -e "$declined" ] && exit 0

base=$(basename "$newest" .md)
stamp=${base#"$project"-}  # YYYY-MM-DD-HHMM
day=${stamp%-*}
clock=${stamp##*-}

when=$stamp
if [ ${#clock} = 4 ]; then
  when="$day ${clock:0:2}:${clock:2:2}"
fi

age=''
if epoch=$(date -d "$day" +%s 2>/dev/null); then
  days=$(( ($(date +%s) - epoch) / 86400 ))
  case "$days" in
    0) age='今日' ;;
    1) age='昨日' ;;
    *) age="${days}日前" ;;
  esac
fi

context="前回のセッションは create-handoff で終わっていて、このプロジェクト（${project}）宛ての引継ぎファイルが残っている: ${newest}（${when}${age:+、$age}）。"
context+='このセッションでは、作業に入る前にまず、その引継ぎを読むかどうかがユーザーに尋ねられる。読むと言われたときにだけ read-handoff スキルが実行される。断られたら、この件はもう持ち出さない。'
context+="断られたとき（/decline-handoff-suggestion での返事も含む）は \`touch ${declined}\` でその印が残され、この引継ぎは次のセッションから提案されなくなる。返事がないまま別の話に進んだときは、印は残されない。"
context+='ユーザーが最初から別の用件を出している場合は、その用件を先に片付けてから尋ねてよい。引継ぎの中身は、尋ねる前には読まない。'

if command -v jq >/dev/null 2>&1; then
  jq -nc --arg ctx "$context" \
    '{hookSpecificOutput: {hookEventName: "SessionStart", additionalContext: $ctx}}'
else
  # SessionStart also accepts plain-text stdout as context.
  printf '%s\n' "$context"
fi
