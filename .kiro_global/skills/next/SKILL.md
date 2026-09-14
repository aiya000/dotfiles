---
name: next
description: Resume and continue the work that was about to happen when the assistant stalled mid-response. Use when the assistant stopped without acting, emitted a broken or incomplete tool call, or the user says the assistant "stopped" / "止まってる" / invokes /next.
allowed-tools: Read, Bash
---

# next

Recover from a stalled turn and correctly continue the work the assistant was about to perform.

## When to use

Use this skill when the previous assistant turn failed to make progress, typically because:

- The response ended with prose that *described* a tool call (e.g. stray `<invoke>` text, a word like `course`) instead of actually invoking a tool
- A tool call was malformed or incomplete, so nothing executed and the turn appears "stopped"
- The assistant narrated an intended next action but never carried it out
- The user reports the assistant is stuck with phrases like "止まってる", "止まってるよ", "stopped", "why did it stop", "続けて", or invokes `/next`

**This skill is not only user-invoked.** Trigger it autonomously whenever the stall pattern above is detected — at the start of a turn (looking back at the previous turn) or before ending a turn (self-check). See "Autonomous self-detection" below.

## Root cause reminder

The most common cause is a broken tool call: the assistant wrote the *text* of a tool invocation into the message body without emitting a real, well-formed tool call. The turn then ends with no side effects, looking frozen to the user.

**The fix is not to re-explain — it is to actually execute the intended action with a correct tool call.**

## Autonomous self-detection (no `/next` needed)

Do not wait for the user to invoke `/next` or say "止まってる". Detect the stall yourself and recover automatically.

- **Before ending any turn, self-check:** did I actually emit real tool calls for every action I described, or did I write tool-call text (`<invoke>`, `<parameter>`, stray marker words like `course`) into prose? If any intended action did not run, do not end the turn — execute it now with a real tool call.
- **At the start of every turn, look back one step:** if the previous assistant turn narrated an action or contained tool-call fragments but produced no matching tool result, treat this exact situation as an implicit `/next` and run this skill's Behavior immediately, without being asked.
- **Recurrence rule:** the moment the same stall pattern appears again, self-trigger this skill. Never rely on the user to notice and re-invoke it.

## Behavior

1. **Identify the intended next action.** Re-read the last few assistant messages and locate the action that was described but not executed. Look specifically for:
    - Prose containing tool-call fragments (`<invoke>`, `<parameter>`, stray marker words)
    - A stated plan ("次はXする", "let me run Y") with no matching tool result afterward
2. **Reconstruct the exact operation.** Determine the concrete command, file edit, or read that was supposed to happen, including its arguments.
3. **Verify current state before acting.** Cheap, non-destructive checks first (e.g. `git status`, read the target file) to confirm the intended action hasn't already partially happened, so it is not done twice.
4. **Execute it properly.** Emit a real, well-formed tool call — never describe a tool call in prose.
5. **Continue the original task** through to its natural completion, following whatever skill or workflow was in progress (e.g. `git-commit`, `git-add`).

## Guardrails

- **Never re-run a destructive or non-idempotent action without checking first** whether it already succeeded. If the prior turn's outcome is unknown (e.g. a commit or push may or may not have gone through), inspect state (`git log`, `git status`) before repeating it.
- Preserve the original intent. `/next` resumes the *previous* task; it does not start a new one or change scope.
- If the intended action genuinely cannot be determined from context, ask the user what they want continued rather than guessing.
- Keep following any skill that was mid-execution, including its safety steps (secret scans, identity checks, confirmations). Do not skip a skill's rules just because you are resuming.

## Examples

### Broken tool call in prior turn

```
Assistant (previous): ...secret scanは済んでるので、stageするのです。
  course
  <invoke name="shell">...git add...</invoke>
User: 止まってるよ
Assistant (/next): (recognizes the git add never ran) Runs `git status` to check,
  then issues a real `git add` tool call and continues the git-commit flow.
```

### Narrated plan, no execution

```
Assistant (previous): 次に build を走らせて確認するのです。
User: /next
Assistant (/next): Actually runs the build command as a proper tool call and reports the result.
```

### Ambiguous

```
User: /next
Assistant: (no clear pending action in recent context) Asks: どの作業の続きをすればいいのです？
```
