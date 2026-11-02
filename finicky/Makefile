# Makefile for the "open in main Chrome window" helper app.
#
# `make install` builds a small .app wrapper from OpenInMainWindow.applescript
# and installs it to $(APP_DIR). Finicky points at this .app as a "browser",
# and the app forwards the URL to open-in-main-window.sh.
#
# The .app is a build artifact and is intentionally NOT committed to the repo;
# only the sources (the .applescript, the .sh and this Makefile) are tracked.

# Directory of this Makefile (the dotfiles finicky/ dir), without trailing slash.
HERE := $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))

APP_NAME := OpenInMainWindow
SRC      := $(HERE)/$(APP_NAME).applescript
SCRIPT   := $(HERE)/open-in-main-window.sh

# Where the built .app is installed. Override with: make install APP_DIR=...
APP_DIR  ?= $(HOME)/Applications
APP      := $(APP_DIR)/$(APP_NAME).app

# Temporary source with the real script path injected.
BUILD_DIR := $(HERE)/.build
GEN_SRC   := $(BUILD_DIR)/$(APP_NAME).applescript

.PHONY: all build install uninstall clean print-path

all: install

# Build the .app into $(APP_DIR), injecting the absolute path of the shell
# script and registering http/https URL schemes so macOS/Finicky can use it
# as a URL handler.
install: $(SRC) $(SCRIPT)
	@mkdir -p "$(BUILD_DIR)" "$(APP_DIR)"
	@sed 's|@SCRIPT_PATH@|$(SCRIPT)|g' "$(SRC)" > "$(GEN_SRC)"
	@chmod +x "$(SCRIPT)"
	@rm -rf "$(APP)"
	osacompile -o "$(APP)" "$(GEN_SRC)"
	@/usr/libexec/PlistBuddy \
		-c "Add :CFBundleIdentifier string com.aiya000.openinmainwindow" \
		-c "Add :CFBundleURLTypes array" \
		-c "Add :CFBundleURLTypes:0 dict" \
		-c "Add :CFBundleURLTypes:0:CFBundleURLName string HTTP and HTTPS URLs" \
		-c "Add :CFBundleURLTypes:0:CFBundleURLSchemes array" \
		-c "Add :CFBundleURLTypes:0:CFBundleURLSchemes:0 string http" \
		-c "Add :CFBundleURLTypes:0:CFBundleURLSchemes:1 string https" \
		"$(APP)/Contents/Info.plist" >/dev/null
	@# Register the app with Launch Services so Finicky can find it by name/path.
	@/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -f "$(APP)"
	@# Re-sign AFTER editing Info.plist, so the signature seals the final bundle.
	@# osacompile signs the app, but our PlistBuddy edits above invalidate that
	@# signature; without this step macOS refuses to prompt for Automation access.
	@codesign --force --deep --sign - "$(APP)"
	@echo "Installed: $(APP)"
	@echo "Point Finicky's browser at this path:"
	@echo "  $(APP)"

uninstall:
	@rm -rf "$(APP)"
	@echo "Removed: $(APP)"

clean:
	@rm -rf "$(BUILD_DIR)"
	@echo "Cleaned: $(BUILD_DIR)"

# Print the path of the installed .app (handy for the Finicky config).
print-path:
	@echo "$(APP)"
