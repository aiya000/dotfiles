#!/usr/bin/env bash
# Lists the places in a skill that assume the local machine, so each one can be
# judged against references/cloud-adaptation-tips.md before the skill is packaged
# for claude.ai.
#
# Usage: audit.sh SKILL_DIR...
#
# Output: one finding per line, tab-separated:
#   <skill>  <CATEGORY>  <file>:<line>  <matched text>
# followed by a per-skill summary. A finding is a place to look at, not a
# verdict: a `~/` inside an example of what malicious code reads is fine.
#
# Categories (the tips file has a section for each):
#   FRONTMATTER  a key claude.ai rejects on upload, or a name/description rule broken
#   HOME-PATH    ~/..., $HOME, /home/<x>, /Users/<x>, /mnt/c -- the machine's own layout
#   STATE        files meant to outlive the session (~/tmp, ~/.ai-memory, ~/.claude/...)
#   FORGE-CLI    gh / glab -- not installed in cloud sessions; GitHub goes through MCP tools
#   HOST-ONLY    tools that need the user's OS or devices (wslview, powershell.exe, adb, ...)
#   LOCAL-TOOL   commands from the user's own toolbox (rm-dust, fd, bash-toys, claude -p)
#   ENV          environment variables the skill reads; nothing sets them in the cloud
#   SANDBOX-HOOK instructions that exist because of the local sandbox or local hooks
#   NETWORK      outbound hosts; the cloud environment's network policy may block them
#   SKILL-DEP    other skills this one calls; they must be converted and uploaded too
set -uo pipefail

if [ $# -eq 0 ]; then
  echo "usage: $(basename "$0") SKILL_DIR..." >&2
  exit 2
fi

# Files meant to outlive the session. HOME-PATH leaves these lines to STATE.
state_re='~/tmp|/tmp/|\.ai-memory|~/\.claude/|~/\.dotfiles|\.cache/|mktemp|\.lock\b|LOCKFILE'

# Variables every shell has, so a skill reading them is not a finding.
common_env='HOME|PWD|PATH|TMPDIR|USER|SHELL|LANG|OLDPWD|IFS|RANDOM|LINENO|PPID|UID|EUID'

emit() { # skill category file line text
  printf '%s\t%s\t%s:%s\t%s\n' "$1" "$2" "$3" "$4" "$(printf '%s' "$5" | cut -c1-160)"
}

scan() { # skill category regex file...
  local skill=$1 category=$2 regex=$3
  shift 3
  local hit file line text
  while IFS= read -r hit; do
    file=${hit%%:*}; hit=${hit#*:}
    line=${hit%%:*}; text=${hit#*:}
    emit "$skill" "$category" "$file" "$line" "$text"
  done < <(grep -nHE -- "$regex" "$@" 2>/dev/null)
}

for dir in "$@"; do
  dir=${dir%/}
  if [ ! -f "$dir/SKILL.md" ]; then
    echo "skip: $dir has no SKILL.md" >&2
    continue
  fi
  skill=$(basename "$(cd "$dir" && pwd -P)")
  mapfile -t files < <(cd "$dir" && grep -rIl -- '' . 2>/dev/null | sed 's|^\./||' | sort)
  files=("${files[@]/#/$dir/}")

  # --- FRONTMATTER: the same rules claude.ai applies on upload
  keys=$(awk 'NR==1 && /^---/ {p=1; next} p && /^---/ {exit} p && /^[A-Za-z_-]+:/ {sub(/:.*/, ""); print}' "$dir/SKILL.md")
  for key in $keys; do
    case $key in
      name|description|allowed-tools|compatibility|license|metadata) ;;
      *) emit "$skill" FRONTMATTER "$dir/SKILL.md" 1 "key '$key' is rejected on upload (move it under metadata: or drop it)" ;;
    esac
  done
  name=$(awk '/^name:/ {sub(/^name:[ \t]*/, ""); gsub(/["\047]/, ""); print; exit}' "$dir/SKILL.md")
  [ "$name" = "$skill" ] || emit "$skill" FRONTMATTER "$dir/SKILL.md" 1 "name '$name' differs from the folder name '$skill'"
  desc=$(awk '/^description:/ {sub(/^description:[ \t]*/, ""); print; exit}' "$dir/SKILL.md")
  [ "${#desc}" -le 1024 ] || emit "$skill" FRONTMATTER "$dir/SKILL.md" 1 "description is ${#desc} chars (max 1024)"
  case $desc in *'<'*|*'>'*) emit "$skill" FRONTMATTER "$dir/SKILL.md" 1 "description contains < or >" ;; esac

  scan "$skill" STATE        "$state_re" "${files[@]}"
  scan "$skill" HOME-PATH    '(^|[^A-Za-z0-9_])~/|\$\{?HOME\b|/home/[a-z]|/Users/[A-Za-z]|/mnt/[a-z]/' "${files[@]}" |
    grep -vE "$state_re"
  scan "$skill" FORGE-CLI    '(^|[^A-Za-z0-9_-])(gh|glab) +(issue|pr|api|auth|repo|release|run|label|mr)\b|Bash\((gh|glab)[ :]' "${files[@]}"
  scan "$skill" HOST-ONLY    '\b(wslview|xdg-open|powershell\.exe|cmd\.exe|explorer\.exe|osascript|pbcopy|pbpaste|clip\.exe|notify-send|adb|scrcpy|systemctl|launchctl|WSLg?)\b|Bash\(open[ :]' "${files[@]}"
  scan "$skill" LOCAL-TOOL   '\b(rm-dust|bash-toys)\b|(^|[^A-Za-z0-9_-])fd +[-A-Za-z.]|Bash\(fd[ :]|claude -p\b' "${files[@]}"
  scan "$skill" SANDBOX-HOOK 'dangerouslyDisableSandbox|[Ss]andbox|\bhooks?\b|PreToolUse|PostToolUse|SessionStart|PreCompact' "${files[@]}"
  scan "$skill" NETWORK      '\b(curl|wget|WebFetch|WebSearch)\b|https?://api\.' "${files[@]}"

  # --- ENV: $FOO / ${FOO}, minus the ones every shell has and the ones the
  # skill assigns itself (FOO=...), which are local names, not inputs
  # (`export FOO=...` is how a skill tells the user to supply an input, so it
  # does not count as the skill assigning it)
  assigned=$(grep -hvE '\bexport ' "${files[@]}" 2>/dev/null |
    grep -oE '(^|[^A-Za-z0-9_$])[A-Z][A-Z0-9_]+=' |
    sed -E 's/^[^A-Z]//; s/=$//' | sort -u | paste -sd'|' -)
  while IFS= read -r hit; do
    var=$(printf '%s' "$hit" | grep -oE '\$\{?[A-Z][A-Z0-9_]+' | head -1 | tr -d '${')
    printf '%s\n' "$var" | grep -qxE "$common_env" && continue
    [ -n "$assigned" ] && printf '%s\n' "$var" | grep -qxE "$assigned" && continue
    file=${hit%%:*}; hit=${hit#*:}
    line=${hit%%:*}; text=${hit#*:}
    emit "$skill" ENV "$file" "$line" "\$$var: $text"
  done < <(grep -nHE '\$\{?[A-Z][A-Z0-9_]+' "${files[@]}" 2>/dev/null)

  # --- SKILL-DEP: Skill(x) in allowed-tools, and "the `x` skill" / `/x` in prose,
  # kept only when x is a sibling skill directory
  parent=$(dirname "$dir")
  grep -hoE 'Skill\([a-z0-9-]+\)|the `[a-z0-9-]+` skill|`/[a-z0-9-]+`' "${files[@]}" 2>/dev/null |
    sed -E 's/^Skill\(//; s/\)$//; s/^the `//; s/` skill$//; s/^`\///; s/`$//' |
    sort -u | while IFS= read -r dep; do
      [ "$dep" = "$skill" ] && continue
      [ -f "$parent/$dep/SKILL.md" ] && emit "$skill" SKILL-DEP "$dir/SKILL.md" - "depends on '$dep'"
    done
done | tee >(awk -F'\t' '
  { n[$1]++; c[$1 FS $2]++ }
  END {
    print "" > "/dev/stderr"
    print "# summary (findings per category)" > "/dev/stderr"
    for (k in c) { split(k, a, FS); line[a[1]] = line[a[1]] " " a[2] "=" c[k] }
    for (s in n) printf "%-40s %3d %s\n", s, n[s], line[s] > "/dev/stderr"
  }')
wait
