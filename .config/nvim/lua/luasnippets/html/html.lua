local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node
local rep = require('luasnip.extras').rep

return list.concat(
  -- Syntax
  sm({ 'tagc', 'tag_closed' }, fmt([[<{} />]], { i(1, 'name') })),
  sm({ 'tag', 'tag_opened' }, fmt([[<{}>{}</{}>]], { i(1, 'name'), i(2, 'here'), rep(1) })),
  sm({ 'com', 'comment' }, fmt([[<!-- {} -->]], { i(1, 'here') })),

  -- Tags
  {
    s('a', fmt([[<a>{}</a>]], { i(1, '') })),
    s('p', fmt([[<p>{}</p>]], { i(1, '') })),
    s('p_close', t('</p>')),
    s('br', t('<br />')),
    s('hr', t('<hr />')),
    s('ol', fmt([[<ol>{}</ol>]], { i(1, 'TARGET') })),
    s('li', fmt([[<li>{}</li>]], { i(1, 'TARGET') })),
    s('img', fmt([[<img src="{}" alt="{}" />]], { i(1, ''), i(2, '') })),
    s('div', fmt([[<div>{}</div>]], { i(1, 'TARGET') })),
    s('span', fmt([[<span>{}</span>]], { i(1, '') })),
    s('section', fmt([[<section>{}</section>]], { i(1, 'here') })),
    s('ul', fmt([[<ul>{}</ul>]], { i(1, '') })),
    s('precode', fmt([[<pre><code>{}</code></pre>]], { i(1, 'here') })),
    s('input', fmt([[<input type="{}" />]], { i(1, '') })),
    s('form', fmt([[<form>{}</form>]], { i(1, '') })),
    s('button', fmt([[<button>{}</button>]], { i(1, '') })),
    s('h1', fmt([[<h1>{}</h1>]], { i(1, '') })),
    s('h2', fmt([[<h2>{}</h2>]], { i(1, '') })),
    s('h3', fmt([[<h3>{}</h3>]], { i(1, '') })),
    s('h4', fmt([[<h4>{}</h4>]], { i(1, '') })),
    s('h5', fmt([[<h5>{}</h5>]], { i(1, '') })),
    s('table', fmt([[<table>{}</table>]], { i(1, '#:here') })),
    s('thead', fmt([[<thead>{}</thead>]], { i(1, '#:here') })),
    s('tr', fmt([[<tr>{}</tr>]], { i(1, '#:here') })),
    s('th', fmt([[<th>{}</th>]], { i(1, '#:here') })),
    s('tbody', fmt([[<tbody>{}</tbody>]], { i(1, '#:here') })),
    s('td', fmt([[<td>{}</td>]], { i(1, '#:here') })),
    s('select', fmt([[<select>{}</select>]], { i(1, '#:here') })),
    s('option', fmt([[<option>{}</option>]], { i(1, '#:here') })),
    s('details', fmt([[<details>{}</details>]], { i(1, 'details') })),
    s('summary', fmt([[<summary>{}</summary>]], { i(1, 'summary') })),
    s('nav', fmt([[<nav>{}</nav>]], { i(1, '#:here') })),
  },

  -- Templates
  sm({ 'css_load', 'link' }, fmt([[<link rel="{}" href="{}">]], { i(1, 'stylesheet'), i(2, 'index.css') })),
  sm({ 'details_summary', 'folding' }, {
    t({ '<details>', '<summary>' }),
    i(1, 'summary'),
    t({ '</summary> <!-- {{{ -->', '' }),
    i(2, ''),
    t({ '', '<!-- }}} -->', '</details>' }),
  }),
  sm({ 'details_summary_without_folding', 'folding_without_folding' }, {
    t({ '<details>', '<summary>' }),
    i(1, 'summary'),
    t({ '</summary>', '' }),
    i(2, ''),
    t({ '', '</details>' }),
  }),
  sm({ 'speaker_notes', 'revealjs_speaker_notes' }, fmt([[<aside class="notes">{}</aside>]], { i(1, 'here') })),

  {
    s('script', fmt([[<script type="text/javascript">{}</script>]], { i(1, 'TARGET') })),
    s('script_load', fmt([[<script src="{}"></script>]], { i(1, 'TARGET') })),
    s('ahref', fmt([[<a href="{}">{}</a>]], { i(1, ''), i(2, 'TARGET') })),
    s('aname', fmt([[<a name="{}">{}</a>]], { i(1, ''), i(2, 'TARGET') })),
    s('tag_close', fmt([[</{}>]], { i(1, 'name') })),
    s('div_close', t('</div>')),
  },

  -- Others
  {
    s('html_half_space', t('&ensp;')),
    s('html_large_space', t('&emsp;')),
    s('html_small_space', t('&thinsp;')),
  }
)
