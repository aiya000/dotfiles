/**
 * Normal mode
 */

mapkey('gT', '', function () {
  RUNTIME('previousTab');
});

mapkey('gt', '', function () {
  RUNTIME('nextTab');
});

mapkey('gh', '', function () {
  openLink('https://google.co.jp');
});

mapkey('gH', '', function () {
  tabOpenLink('https://google.co.jp');
});

mapkey('<Ctrl-b>', '', function () {
  Normal.scroll('pageUp');
});

mapkey('<Ctrl-f>', '', function () {
  Normal.scroll('pageDown');
});

mapkey('d', '#3Close current tab', function () {
  RUNTIME('closeTab');
});

mapkey('u', '#3Restore closed tab', function () {
  RUNTIME('openLast');
});

mapkey('H', '#4Go back in history', function () {
  history.go(-1);
}, {repeatIgnore: true});

mapkey('L', '#4Go forward in history', function () {
  history.go(1);
}, {repeatIgnore: true});

map('F', 'gf');
map('r', 'F5');
map('b', 'T');
// map('o', ???);
map('<Ctrl-p>', 'gT');
map('<Ctrl-n>', 'gt');

/**
 * Insert mode
 */

imap('<Ctrl-[>', '<Esc>');
imap('<Ctrl-l>', '<Esc>');

imap('<Ctrl-a>', '<Home>');
imap('<Ctrl-e>', '<End>');
imap('<Ctrl-b>', '<Left>');
imapkey('<Ctrl-f>', 'Move the cursor forward 1', function() {  // {{{
  var element = getRealEdit();
  if (element.setSelectionRange !== undefined) {
    var pos = element.selectionStart + 1;
    element.setSelectionRange(pos, pos);
  } else {
    // for contenteditable div
    document.getSelection().modify('move', 'right', 'character');
  }
});

// }}}

imap('<Ctrl-w>', '<Alt-w>');
imap('<Ctrl-h>', '<Alt-h>');
imapkey('<Ctrl-u>', '', killLineBefore);
imapkey('<Ctrl-k>', '', killLineAfter);

imapkey('<Ctrl-g>', '', showEditor);

function killLineBefore() {
  var element = getRealEdit();

  if (element.value === '') {
    return;
  }

  element.value = element.value.substr(element.selectionStart);
  element.setSelectionRange(0, 0);
}

function killLineAfter() {
  var element = getRealEdit();
  element.value = element.value.substr(0, element.selectionStart);
  element.setSelectionRange(element.selectionStart, 0);
}

function showEditor() {
  var element = getRealEdit();
  element.blur();
  Insert.exit();
  Front.showEditor(element);
}

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
