---
name: verify-git-command-location
description: Verify that the correct git binary is in use, especially when running under WSL with a Windows filesystem. Use the first time a git command is executed in a session.
allowed-tools: Bash(git rev-parse:*), Bash(pwd:*), Bash(which git:*), Bash(ls:*)
---

# check-git-on-windows

When the working directory is on a Windows filesystem, WSL's native `git` may fail with errors.
Avoid this by using the appropriate git binary.

## Step 1: Check if you are on a Windows filesystem

Run `git rev-parse --show-toplevel`.

If the output starts with a Windows drive letter (`C:`, `D:`, etc.), you are on a Windows filesystem.
If it shows a normal UNIX path (e.g. `/home/...`), stop here — the rest of this skill does not apply.

## Step 2: Confirm the current directory is under `/mnt/c/`

Run `pwd`.

The current working directory should start with `/mnt/c/` or a similar WSL mount path.
If it does not, notify the user.

## Step 3: Check where the git command is from

Run `which git`.

If the location is **not** in a user-placed path like `~/bin/git`, stop here.

If it is in a user-placed path, check whether it looks like a WSL-to-Windows filesystem bridge script:

Run `ls -l $(which git)`.

```bash
# Example of a WSL-to-Windows filesystem bridge script
lrwxrwxrwx 1 aiya000 aiya000 80 Jan 25 12:17 /home/aiya000/bin/git -> /home/aiya000/Repository/git_bridge_wsl2_and_windows/git_bridge_wsl2_and_windows
```

If it does **not** appear to be such a bridge script, stop here.

## Last: If no bridge is found

If no suitable method is found, notify the user and explain what they need to set up.
