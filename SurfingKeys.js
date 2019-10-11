/**
 * Please see `dispatchMouseClick()` of Surfingkeys/content_scripts/hints.js
 */
function dispatchMouseClick(element) {
  Hints.flashPressedLink(element);
  tabOpenLink(element.href);
}

mapkey('gT', '', function () {
  RUNTIME('previousTab');
});

mapkey('gt', '', function () {
  RUNTIME('nextTab');
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

mapkey('F', '', function () {
  Hints.create('', dispatchMouseClick);
});

map('r', 'F5');
map('b', 't');
// map('o', ???);
map('<Ctrl-p>', 'gT');
map('<Ctrl-n>', 'gt');

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
  .sk_theme #sk_omnibarSearchResult>ul>li:nth-child(odd) {
    background: #303030;
  }
  .sk_theme #sk_omnibarSearchResult>ul>li.focused {
    background: #3e4452;
  }
  #sk_status, #sk_find {
    font-size: 20pt;
  }
`;
