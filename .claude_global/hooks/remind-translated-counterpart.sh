#!/usr/bin/env bash
# PostToolUse (Write|Edit): when the markdown file just written has a
# translated counterpart sitting beside it, say so, so the pair does not drift.
#
# The trigger is the counterpart EXISTING ON DISK, never the filename alone.
# A repository with only a README.md and no translation beside it never hears
# from this hook: whether a project is translated at all is the author's
# decision, not something to infer from a name. So this is safe to install
# globally -- it is silent in every single-language repository.

set -uo pipefail

payload=$(cat)
file=$(printf '%s' "$payload" | jq -r '.tool_input.file_path // .tool_response.filePath // empty' 2>/dev/null)
[ -n "$file" ] || exit 0

case "$file" in
  *.md | *.markdown) ;;
  *) exit 0 ;;
esac

dir=$(dirname -- "$file")
base=$(basename -- "$file")
ext="${base##*.}"
stem="${base%.*}"

# Suffixes that mark which language a half is written in.
markers='_JP _JA _jp _ja _EN _en -JP -ja -jp -en .jp .ja .en'

# If the edited file is itself a marked half, its counterpart is the bare name.
bare=''
for m in $markers; do
  case "$stem" in
    *"$m") bare="${stem%"$m"}" ;;
  esac
done

candidates=''
if [ -n "$bare" ]; then
  candidates="$bare"
  for m in $markers; do candidates="$candidates $bare$m"; done
else
  for m in $markers; do candidates="$candidates $stem$m"; done
fi

counterpart=''
for c in $candidates; do
  if [ "$c" != "$stem" ] && [ -f "$dir/$c.$ext" ]; then
    counterpart="$c.$ext"
    break
  fi
done

[ -n "$counterpart" ] || exit 0

jq -n --arg edited "$base" --arg other "$counterpart" '{
  hookSpecificOutput: {
    hookEventName: "PostToolUse",
    additionalContext: ("\($edited) has a translated counterpart beside it: \($other). The two are one document written twice, so make the same change in \($other) as well, before finishing and in the same commit -- the sync-translated-docs skill says what has to match and what may legitimately differ. Ignore this if you have already updated it, or if this edit only touched the cross-language link line.")
  }
}'
