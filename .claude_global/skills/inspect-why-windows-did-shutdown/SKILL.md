---
name: inspect-why-windows-did-shutdown
description: Find out why Windows shut down or rebooted, by reading the Windows System event log from WSL via powershell.exe. Use when the user says Windows went down, restarted on its own, crashed, or asks whether it was a Windows Update or a bug.
allowed-tools: Bash(powershell.exe:*), Bash(command -v:*)
---

# inspect-why-windows-did-shutdown

The Windows System event log already knows why the machine went down.
Read it from WSL with `powershell.exe` -- there is no need to open the GUI, and no need to ask
the user to go clicking.

**WSL itself holds no record of this.** WSL is torn down with the host, so `uptime`, `last`, and
`journalctl` inside the distro say nothing about why Windows restarted. Always go to the Windows
log.

## Step 0: Confirm `powershell.exe` is reachable

```sh
command -v powershell.exe
```

If it is missing, WSL interop is off. Stop here and fall back to
[the GUI instructions](#if-you-cannot-reach-powershellexe).

## Step 1: Always set the output encoding first

`powershell.exe` writes the log's Japanese text in Shift-JIS, which arrives as mojibake.
**Every** command in this skill must start with:

```powershell
[Console]::OutputEncoding=[Text.Encoding]::UTF8;
```

Strip the CR too, or the output is littered with `\r`: pipe through `tr -d '\r'`.

## Step 2: Pull the shutdown events

```sh
powershell.exe -NoProfile -Command "[Console]::OutputEncoding=[Text.Encoding]::UTF8; Get-WinEvent -FilterHashtable @{LogName='System'; Id=41,1074,6008,1001; StartTime=(Get-Date).AddDays(-3)} -ErrorAction SilentlyContinue | Sort-Object TimeCreated | ForEach-Object { '--- {0} / Id={1} / {2}' -f \$_.TimeCreated, \$_.Id, \$_.ProviderName; \$_.Message }" 2>&1 | tr -d '\r'
```

Reading the System log does not need Administrator.

`-ErrorAction SilentlyContinue` matters: `Get-WinEvent -FilterHashtable` **throws** when nothing
matches, instead of returning empty. Without it, a clean machine looks like a failed command.

Widen `AddDays(-3)` if the event is older than the window.

## Step 3: Read the four IDs

| ID | Provider | What it means |
|---|---|---|
| **1074** | User32 | Who initiated the shutdown/restart, and why. The one that answers the question |
| **6008** | EventLog | The previous shutdown was unexpected -- power loss, hard freeze, held power button |
| **41** | Kernel-Power | The system rebooted without shutting down cleanly |
| **1001** | BugCheck | A blue screen, with the stop code and dump path |

**A 1074 with none of 41 / 6008 / 1001 means it was a planned shutdown, not a crash.** Say so
plainly -- that is usually the reassurance the user is actually after.

Read the `Reason Code` in the 1074 message:

- `0x80020003` -- `オペレーティング システム: アップグレード (計画済)`. Windows Update. The
  initiating process is `C:\WINDOWS\servicing\TrustedInstaller.exe` on behalf of
  `NT AUTHORITY\SYSTEM`
- `0x5000000` / `0x500ff` -- an application or the user asked for it. The process path in the
  message names which
- A `1074` whose process is `StartMenuExperienceHost.exe` is the user picking shutdown from the
  Start menu

**Windows Update restarts often appear twice, a minute apart** -- the update applies in more than
one phase. Two adjacent 1074s are not two separate incidents.

## Step 4: If it was Windows Update, name the update

```sh
powershell.exe -NoProfile -Command "[Console]::OutputEncoding=[Text.Encoding]::UTF8; Get-HotFix | Sort-Object InstalledOn -Descending | Select-Object -First 5 HotFixID, Description, InstalledOn | Format-Table -AutoSize" 2>&1 | tr -d '\r'
```

The KB installed just before the restart is the culprit.

## Step 5: If it was a crash, go after the dump

Only when 41, 6008, or 1001 showed up.

The 1001 message carries the stop code and the dump path. Confirm the dump exists:

```sh
powershell.exe -NoProfile -Command "[Console]::OutputEncoding=[Text.Encoding]::UTF8; Get-ChildItem C:\Windows\Minidump -ErrorAction SilentlyContinue | Sort-Object LastWriteTime -Descending | Select-Object -First 5 Name, LastWriteTime, Length | Format-Table -AutoSize" 2>&1 | tr -d '\r'
```

A **41 with no 1001 and no minidump** is the hardware-side pattern: power loss, PSU, overheating,
or a hard hang. There is no software stack trace to find, so do not go looking for one.

## Step 6: Bracket the outage with the service events

Useful when the user wants to know how long the machine was gone:

```sh
powershell.exe -NoProfile -Command "[Console]::OutputEncoding=[Text.Encoding]::UTF8; Get-WinEvent -FilterHashtable @{LogName='System'; Id=6005,6006,6013; StartTime=(Get-Date).AddDays(-3)} -ErrorAction SilentlyContinue | Sort-Object TimeCreated | Format-Table -AutoSize TimeCreated, Id, @{n='Msg';e={(\$_.Message -split \"\`r?\`n\")[0]}}" 2>&1 | tr -d '\r'
```

- `6006` -- the event log service stopped, i.e. the machine started going down
- `6005` -- it started again, i.e. the machine came back
- `6013` -- uptime in seconds, logged daily. A tiny value right after a boot is normal, not a sign
  of instability

## Reporting back

Lead with the verdict -- planned update, user action, or genuine crash -- then the timestamps, then
the evidence. The user asked a yes/no question ("was it an update, or a bug?"); answer that first.

If it was a planned update restart and the user was working at the time, mention **アクティブ時間**
(Settings -> Windows Update -> Advanced options -> Active hours) once. Do not belabour it.

## If you cannot reach `powershell.exe`

Hand the user the GUI path:

1. `Win + R` -> `eventvwr.msc`
2. **Windows ログ -> システム**
3. Right pane -> **現在のログをフィルター** -> enter `41,1074,6008,1001` under イベント ID

## After the machine came back

A host restart takes every local service down with it. If this repository has a startup runbook
(a `startup` skill, a `Makefile`, an `AGENTS.md` recovery section), offer to bring the stack back
up -- the user is usually here because something they rely on stopped answering.
