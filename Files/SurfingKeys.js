/**
 * General
 */

try {
  Hints.characters = 'wertyuiopasdfghjkzxcvbm';
  settings.hintAlign = 'left';
  addSearchAlias('g', 'google', 'https://www.google.com/search?q=');
} catch (e) {
  throw new Error(`In the section 'General': ${e}`);
}

/**
 * Normal mode
 */
try {
  mapkey('gT', 'previous tab', function () {  // {{{
    RUNTIME('previousTab');
  });  // }}}
  mapkey('gt', 'next tab', function () {  // {{{
    RUNTIME('nextTab');
  });  // }}}
  mapkey('gh', 'open link google', function () {  // {{{
    openLink('https://google.co.jp');
  });  // }}}
  mapkey('gH', 'tab open link google', function () {  // {{{
    tabOpenLink('https://google.co.jp');
  });  // }}}
  mapkey('<Ctrl-b>', 'page up', function () {  // {{{
    Normal.scroll('pageUp');
  });  // }}}
  mapkey('<Ctrl-f>', 'page down', function () {  // {{{
    Normal.scroll('pageDown');
  });  // }}}
  mapkey('d', '#3Close current tab', function () {  // {{{
    RUNTIME('closeTab');
  });  // }}}
  mapkey('u', '#3Restore closed tab', function () {  // {{{
    RUNTIME('openLast');
  });  // }}}
  mapkey('H', '#4Go back in history', function () {  // {{{
    history.go(-1);
  }, { repeatIgnore: true });  // }}}
  mapkey('L', '#4Go forward in history', function () {  // {{{
    history.go(1);
  }, { repeatIgnore: true });  // }}}
  mapkey('F', '#1Open a link in non-active new tab', function () {  // {{{
    Hints.create(',', Hints.dispatchMouseClick, {
      tabbed: true,
      active: false,
    });
  });  // }}}
  mapkey('o', '#8Open a URL in current tab', function () {  // {{{
    Front.openOmnibar({
      type: 'URLs',
      extra: 'getAllSites',
      tabbed: false,
    });
  });  // }}}
  mapkey('b', '#3Choose a tab', function () {  // {{{
    Front.openOmnibar({
      type: 'URLs',
      extra: 'getAllSites',
    });
  });  // }}}
  mapkey('gs', '#12View page source', function () {  // {{{
    RUNTIME('viewSource', {  // {{{
      tab: { tabbed: true },
    });  // }}}
  });  // }}}
  mapkey('<', 'move tab -1', function () {  // {{{
    RUNTIME('moveTab', { step: -1 });
  });  // }}}
  mapkey('>', 'move tab +1', function () {  // {{{
    RUNTIME('moveTab', { step: 1 });
  });  // }}}
  mapkey('t', '#4Edit current URL with vim editor, and open in new tab', function () {  // {{{
    Front.openOmnibar({
      type: 'URLs',
      extra: 'getAllSites',
      tabbed: true,
    });
  });  // }}}
  mapkey('Q', '#11Edit Settings', function () {  // {{{
    tabOpenLink('/pages/options.html');
  });  // }}}
  mapkey('R', '#4Reload the page', function () {  // {{{
    RUNTIME('reloadTab', { nocache: true });
  });  // }}}
  mapkey('W', '#3New window with current tab',  function() {  // {{{
    RUNTIME('newWindow');
  });  // }}}
  map('g_', '$');
  map('<Ctrl-p>', 'gT');
  map('<Ctrl-n>', 'gt');
} catch (e) {
  throw new Error(`In the section 'Normal mode': ${e}`);
}

/**
 * Insert mode
 */

try {
  imap('<Ctrl-[>', '<Esc>');
  imap('<Ctrl-l>', '<Esc>');

  imapkey('<Ctrl-a>', 'Move cursor to the head of line', moveCursor('left', 'lineboundary'));
  imapkey('<Ctrl-e>', 'Move cursor to the last of line', moveCursor('right', 'lineboundary'));
  imapkey('<Ctrl-b>', 'Move cursor to the backword char', moveCursor('left', 'character'));
  imapkey('<Ctrl-f>', 'Move cursor to the forward char', moveCursor('right', 'character'));
  imapkey('<Ctrl-p>', 'Move cursor to the above', moveCursor('left', 'line'));
  imapkey('<Ctrl-n>', 'Move cursor to the below', moveCursor('right', 'line'));

  imapkey('<Ctrl-w>', '', deleteLeftWord);
  imapkey('<Ctrl-h>', '', deleteLeftChar);
  imapkey('<Ctrl-u>', '', killLineBefore);
  imapkey('<Ctrl-k>', '', killLineAfter);
  imapkey('<Ctrl-d>', '', '');

  imapkey('<Ctrl-g>', 'Edit in the editor', editInEditor);

  iunmap('<Ctrl-i>');
} catch (e) {
  throw new Error(`In the section 'Insert mode': ${e}`);
}

function moveCursor(direction, granularity) {
  return () => {
    document.getSelection().modify('move', direction, granularity);
  }
}

function killLineBefore() {
  const element = getRealEdit();
  if (element.value === '') {
    return;
  }

  element.value = element.value.substr(element.selectionStart);
  element.setSelectionRange(0, 0);
}

function killLineAfter() {
  const element = getRealEdit();
  const firstHalf = element.value.substr(0, element.selectionStart);

  const thisLineBreak = element.value.indexOf('\n', element.selectionStart);
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
  document.getSelection().modify('move', 'backward', 'word');
  const v = selection.focusNode.data, p1 = selection.focusOffset;
  selection.focusNode.data = v.substr(0, p1) + v.substr(p0);
  selection.setPosition(selection.focusNode, p1);
}

function deleteLeftChar() {
  document.getSelection()
    .modify('extend', 'backward', 'character');
}

/**
 * Visual mode
 */
try {
  vmap('<Ctrl-[>', '<Esc>');
  vmap('<Ctrl-l>', '<Esc>');
} catch (e) {
  throw new Error(`In the section 'Visual mode': ${e}`);
}

/**
 * Command mode
 */

try {
  cmap('<Ctrl-[>', '<Esc>');
  cmap('<Ctrl-l>', '<Esc>');

  cmap('<Ctrl-a>', '<Home>');
  cmap('<Ctrl-e>', '<End>');
  cmap('<Ctrl-b>', '<Left>');
  cmap('<Ctrl-f>', '<Right>');

  cmap('<Ctrl-w>', '');
  cmap('<Ctrl-h>', '<Alt-h>');
  // cmap('<Ctrl-d>', '?');
  cmap('<Ctrl-u>', '');
  cmap('<Ctrl-k>', '');
} catch (e) {
  throw new Error(`In the section 'Command mode': ${e}`);
}

/**
 * The vim editor
 */

aceVimMap('<Ctrl-j><Ctrl-k>', ':w<CR>', 'normal');

aceVimMap('<Ctrl-[>', '<Esc>', 'insert');
aceVimMap('<Ctrl-j><Ctrl-k>', '<Esc>:w<CR>', 'insert');
aceVimMap('<Ctrl-l>', '<Esc>', 'insert');

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
