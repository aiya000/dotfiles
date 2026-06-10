---
name: inspect-malicious-code
description: Inspect a project for potentially malicious code (malware, spyware, etc.) using static analysis only. Use when the user asks to audit a project or dependency for malicious or suspicious code.
---

# inspect-malicious-code

Inspect a project for potentially malicious code (malware, spyware, etc.).

## Arguments

- Target directory path (optional). If not specified, use current working directory
- `--include-node-modules`: Include `node_modules` directory in inspection (excluded by default)

## Behavior

> **⚠️ CRITICAL: NEVER execute any code during inspection.**
>
> This skill performs **static analysis only**. Do not run, eval, or execute any scripts, commands, or code found in the target project. Malicious code may activate upon execution.

### Step 1: Confirmation

1. Display the target directory path clearly
2. Ask the user: "Do you want to inspect this project for malicious code?"
3. If **yes** → proceed to inspection
4. If **no** → exit

### Step 2: Inspection

Thoroughly examine the codebase for the following suspicious patterns:

#### 2.1 Obfuscated Code

- `eval()` usage with dynamic strings
- `Function()` constructor with string arguments
- Base64 encoded strings (especially long ones)
- Hex-encoded strings
- String concatenation tricks to hide keywords
- Minified code in source files (not build artifacts)

#### 2.2 External Communication

- `fetch()` calls to unknown/suspicious URLs
- `XMLHttpRequest` usage
- `http`/`https` module imports with outbound requests
- WebSocket connections
- DNS lookups to suspicious domains
- Data exfiltration patterns (sending local data externally)

#### 2.3 Environment & Credentials Access

- `process.env` access (especially for sensitive keys like `API_KEY`, `SECRET`, `TOKEN`, `PASSWORD`)
- Reading `.env` files directly
- Accessing system environment variables
- Keychain/credential store access
- SSH key access (`~/.ssh/`)

#### 2.4 File System Access

- Reading sensitive files (`/etc/passwd`, `~/.bashrc`, `~/.zshrc`, browser data directories)
- Writing to system directories
- Recursive directory scanning
- File upload to external servers
- Archive creation of user data

#### 2.5 Process & System Manipulation

- `child_process` usage (especially `exec`, `spawn` with shell commands)
- Modifying PATH or other environment variables
- Installing global packages silently
- Modifying system configuration files
- Persistence mechanisms (cron jobs, startup scripts)

### Step 3: Results Summary

Report findings with severity levels:

- **🔴 DANGER**: Clearly malicious patterns found
- **🟡 WARNING**: Suspicious code that needs manual review
- **🟢 CLEAN**: No suspicious patterns detected

For each finding, provide:

- File path and line number
- The suspicious code snippet
- Explanation of why it's flagged
- Risk assessment

## npm/pnpm/yarn/bun Projects (Special Considerations)

When the target is a Node.js project (has `package.json`):

### Package Scripts Inspection

Check `package.json` for suspicious scripts:

```json
{
  "scripts": {
    "preinstall": "...",
    "install": "...",
    "postinstall": "..."
  }
}
```

These lifecycle scripts run automatically and are common attack vectors.

### Dependency Analysis

- Check for typosquatting (packages with names similar to popular ones)
- Look for packages with very few downloads or recent creation
- Identify packages that request excessive permissions
- Check for known malicious packages (if recognizable)

### Lock File Integrity

- Verify `package-lock.json`, `yarn.lock`, `pnpm-lock.yaml`, or `bun.lockb` exists
- Look for unexpected registry URLs (not npmjs.org)
- Check for integrity hash mismatches if possible

## Output Format

```
## Inspection Target
📁 /path/to/project

## Inspection Scope
- Source files: ✓
- node_modules: ✗ (use --include-node-modules to include)

## Findings

### 🔴 DANGER (X found)
[List of dangerous findings with details]

### 🟡 WARNING (X found)
[List of warnings with details]

### 🟢 Summary
[Overall assessment and recommendations]
```

## Notes

- This inspection is not exhaustive and cannot guarantee detection of all malicious code
- Sophisticated attacks may evade detection
- Always review flagged code manually before making decisions
- When in doubt, consult security professionals
