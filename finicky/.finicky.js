// ~/.finicky.js  (symlinked from ~/.dotfiles/finicky/.finicky.js)
//
// Route every link to the "main" Google Chrome window.
//
// We don't open Chrome directly. Instead we hand the URL to our helper app,
// OpenInMainWindow.app, which runs open-in-main-window.sh. That script finds
// the Chrome window whose FIRST tab is the marker page
//   https://www.google.com/search?q=メインウィンドウ
// and opens the URL in a new tab there (falling back to a new window).
//
// The helper app is referenced by its bundle id (not an absolute path), so
// this config stays portable across machines. Install the app first with:
//   cd ~/.dotfiles/finicky && make install
//
// @ts-check
/**
 * @typedef {import('/Applications/Finicky.app/Contents/Resources/finicky.d.ts').FinickyConfig} FinickyConfig
 */

// Bundle id of the helper app installed by `make install`.
// Keep this in sync with CFBundleIdentifier in the Makefile.
const MAIN_WINDOW_OPENER = "com.aiya000.openinmainwindow";

/** @type {FinickyConfig} */
export default {
  options: {
    // Log every request so we can inspect how URLs are routed.
    logRequests: true,
  },
  // Send every URL to the helper app by default.
  defaultBrowser: {
    name: MAIN_WINDOW_OPENER,
    appType: "bundleId",
  },
  handlers: [],
};
