-- OpenInMainWindow.applescript
--
-- A thin wrapper .app that Finicky treats as a "browser".
-- It receives a URL (via the GetURL / open location Apple event, or as a
-- command-line argument) and forwards it to open-in-main-window.sh, which
-- opens the URL in a new tab of the "main" Google Chrome window.
--
-- The absolute path to open-in-main-window.sh is injected at build time by
-- the Makefile, replacing the @SCRIPT_PATH@ placeholder below.

property scriptPath : "@SCRIPT_PATH@"

on openURL(theURL)
	do shell script quoted form of scriptPath & " " & quoted form of theURL
end openURL

-- Finicky / macOS delivers URLs through the GetURL Apple event.
on open location theURL
	openURL(theURL)
end open location

-- Fallback: allow running with a URL as a command-line argument,
-- e.g. for testing: osascript OpenInMainWindow.app <url>
on run argv
	if (count of argv) is greater than 0 then
		openURL(item 1 of argv)
	end if
end run
