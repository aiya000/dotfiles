const {
  aceVimMap,
  mapkey,
  imap,
  iunmap,
  imapkey,
  getClickableElements,
  vmap,
  vmapkey,
  map,
  unmap,
  cmap,
  addSearchAlias,
  removeSearchAlias,
  tabOpenLink,
  readText,
  Clipboard,
  Hints,
  Visual,
  RUNTIME,
} = api;

/**
 * General
 */

try {
  Hints.characters = "wertuiopasdfghjkzxcvbm";
  settings.hintAlign = "left";
} catch (e) {
  alert(`In the section 'General': ${e}`);
}

/**
 * Normal mode
 */

try {
  /**
   * Noremaps
   */
  map("<Ctrl-b>", "u");
  map("<Ctrl-f>", "d");
  map("F", "af");
  map("d", "x");

  mapkey("gT", "previous tab", () => RUNTIME("previousTab"));
  mapkey("gt", "next tab", () => RUNTIME("nextTab"));
  mapkey("gh", "open link google", () => openLink("https://google.co.jp"));
  mapkey("gH", "tab open link google", () =>
    tabOpenLink("https://google.co.jp"),
  );
  mapkey("u", "#3Restore closed tab", () => RUNTIME("openLast"));
  mapkey("H", "#4Go back in history", () => history.go(-1), {
    repeatIgnore: true,
  });
  mapkey("L", "#4Go forward in history", () => history.go(1), {
    repeatIgnore: true,
  });
  mapkey("o", "#8Open a URL in current tab", () =>
    Front.openOmnibar({
      type: "URLs",
      extra: "getAllSites",
      tabbed: false,
    }),
  );
  mapkey("b", "#3Choose a tab", () =>
    Front.openOmnibar({
      type: "URLs",
      extra: "getAllSites",
    }),
  );
  mapkey("<", "move tab -1", () => RUNTIME("moveTab", { step: -1 }));
  mapkey(">", "move tab +1", () => RUNTIME("moveTab", { step: 1 }));
  mapkey("t", "#4Edit current URL with vim editor, and open in new tab", () =>
    Front.openOmnibar({
      type: "URLs",
      extra: "getAllSites",
      tabbed: true,
    }),
  );
  mapkey("Q", "#11Edit Settings", () => tabOpenLink("/pages/options.html"));
  mapkey("R", "#4Reload the page", () =>
    RUNTIME("reloadTab", { nocache: true }),
  );

  /**
   * Remaps
   */
  map("<Ctrl-n>", "gt");
  map("<Ctrl-p>", "gT");
  map("g_", "$");
} catch (e) {
  alert(`In the section 'Normal mode': ${e}`);
}

/**
 * Insert mode
 */

try {
  imap("<Ctrl-[>", "<Esc>");
  imap("<Ctrl-l>", "<Esc>");
  imapkey(
    "<Ctrl-b>",
    "Move cursor to the backword char",
    moveCursor("left", "character"),
  );
  imapkey(
    "<Ctrl-f>",
    "Move cursor to the forward char",
    moveCursor("right", "character"),
  );
  imapkey("<Ctrl-p>", "Move cursor to the above", moveCursor("left", "line"));
  imapkey("<Ctrl-n>", "Move cursor to the below", moveCursor("right", "line"));
  imapkey("<Ctrl-g>", "Edit in the editor", editInEditor);
  iunmap("<Ctrl-i>");
  iunmap(":"); // Emoji completion
} catch (e) {
  alert(`In the section 'Insert mode': ${e}`);
}

function moveCursor(direction, granularity) {
  return () => {
    document.getSelection().modify("move", direction, granularity);
  };
}

function killLineBefore() {
  const element = getRealEdit();
  if (element.value === "") {
    return;
  }

  element.value = element.value.substr(element.selectionStart);
  element.setSelectionRange(0, 0);
}

function killLineAfter() {
  const element = getRealEdit();
  const firstHalf = element.value.substr(0, element.selectionStart);

  const thisLineBreak = element.value.indexOf("\n", element.selectionStart);
  const lastHalf = element.value.substr(thisLineBreak);

  element.value = firstHalf + lastHalf;
  element.setSelectionRange(firstHalf.length - 1, 0);
}

function editInEditor() {
  const element = getRealEdit();
  element.blur();
  Insert.exit();
  Front.showEditor(element);
}

function deleteLeftWord() {
  const element = getRealEdit();

  if (element.setSelectionRange !== undefined) {
    const pos = deleteNextWord(element.value, -1, element.selectionStart);
    element.value = pos[0];
    element.setSelectionRange(pos[1], pos[1]);
    return;
  }

  // for contenteditable div
  const selection = document.getSelection();
  const p0 = selection.focusOffset;
  document.getSelection().modify("move", "backward", "word");
  const v = selection.focusNode.data;
  const p1 = selection.focusOffset;
  selection.focusNode.data = v.substr(0, p1) + v.substr(p0);
  selection.setPosition(selection.focusNode, p1);
}

function deleteLeftChar() {
  document.getSelection().modify("extend", "backward", "character");
}

/**
 * Visual mode
 */
try {
  // TODO: Enable this when api to be providing this.
  // vmap("<Ctrl-[>", "<Esc>");
  // vmap("<Ctrl-l>", "<Esc>");
} catch (e) {
  alert(`In the section 'Visual mode': ${e}`);
}

/**
 * Command mode
 */

try {
  cmap("<Ctrl-[>", "<Esc>");
  cmap("<Ctrl-l>", "<Esc>");

  cmap("<Ctrl-a>", "<Home>");
  cmap("<Ctrl-e>", "<End>");
  cmap("<Ctrl-b>", "<Left>");
  cmap("<Ctrl-f>", "<Right>");

  cmap("<Ctrl-w>", "");
  cmap("<Ctrl-h>", "<Alt-h>");
  // cmap('<Ctrl-d>', '?');
  cmap("<Ctrl-u>", "");
  cmap("<Ctrl-k>", "");
} catch (e) {
  alert(`In the section 'Command mode': ${e}`);
}

/**
 * The vim editor
 */

aceVimMap("<Ctrl-j><Ctrl-k>", ":w<CR>", "normal");

aceVimMap("<Ctrl-[>", "<Esc>", "insert");
aceVimMap("<Ctrl-j><Ctrl-k>", "<Esc>:w<CR>", "insert");
aceVimMap("<Ctrl-l>", "<Esc>", "insert");

/**
 * Styles
 */

settings.theme = `
  .sk_theme {
    font-family: Input Sans Condensed, Charcoal, sans-serif;
    font-size: 10pt;
    background: #24272e;
    color: #abb2bf;
  }

  .sk_theme tbody {
    color: #fff;
  }

  .sk_theme input {
    color: #d0d0d0;
  }

  .sk_theme .url {
    color: #61afef;
  }

  .sk_theme .annotation {
    color: #56b6c2;
  }

  .sk_theme .omnibar_highlight {
    color: #528bff;
  }

  .sk_theme .omnibar_timestamp {
    color: #e5c07b;
  }

  .sk_theme .omnibar_visitcount {
    color: #98c379;
  }

  .sk_theme #sk_omnibarSearchResult > ul > li:nth-child(odd) {
    background: #303030;
  }

  .sk_theme #sk_omnibarSearchResult > ul > li.focused {
    background: #3e4452;
  }

  #sk_status, #sk_find {
    font-size: 20pt;
  }
`;
