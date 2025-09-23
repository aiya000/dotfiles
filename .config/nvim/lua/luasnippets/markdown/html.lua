local list = require('utils.list')
local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require('luasnip.extras.fmt').fmt

-- Helper function to create snippets with multiple triggers
local function sm(triggers, nodes)
  local snippets = {}
  for _, trigger in ipairs(triggers) do
    table.insert(snippets, s(trigger, nodes))
  end
  return snippets
end

-- Syntax

local html_snippets = {}

vim.list_extend(
  html_snippets,
  sm({ 'tag_closed', 'tagc' }, {
    t('<'),
    i(1, 'name'),
    t(' />'),
  })
)

vim.list_extend(
  html_snippets,
  sm({ 'tag_opened', 'tag' }, {
    t('<'),
    i(1, 'name'),
    t('>'),
    i(2, 'here'),
    t('</'),
    i(1, ''),
    t('>'),
  })
)

vim.list_extend(
  html_snippets,
  sm({ 'comment', 'com' }, {
    t('<!-- '),
    i(1, 'here'),
    t(' -->'),
  })
)

-- Tags
table.insert(
  html_snippets,
  s('a', {
    t('<a>'),
    i(1, ''),
    t('</a>'),
  })
)

table.insert(
  html_snippets,
  s('p', {
    t('<p>'),
    i(1, ''),
    t('</p>'),
  })
)

table.insert(html_snippets, s('p_close', t('</p>')))

table.insert(html_snippets, s('br', t('<br />')))

table.insert(html_snippets, s('hr', t('<hr />')))

table.insert(
  html_snippets,
  s('ol', {
    t('<ol>'),
    i(1, 'TARGET'),
    t('</ol>'),
  })
)

table.insert(
  html_snippets,
  s('li', {
    t('<li>'),
    i(1, 'TARGET'),
    t('</li>'),
  })
)

table.insert(
  html_snippets,
  s('img', {
    t('<img src="'),
    i(1, ''),
    t('" alt="'),
    i(2, ''),
    t('" />'),
  })
)

table.insert(
  html_snippets,
  s('div', {
    t('<div>'),
    i(1, 'TARGET'),
    t('</div>'),
  })
)

table.insert(
  html_snippets,
  s('span', {
    t('<span>'),
    i(1, ''),
    t('</span>'),
  })
)

table.insert(
  html_snippets,
  s('section', {
    t('<section>'),
    i(1, 'here'),
    t('</section>'),
  })
)

table.insert(
  html_snippets,
  s('ul', {
    t('<ul>'),
    i(1, ''),
    t('</ul>'),
  })
)

table.insert(
  html_snippets,
  s('precode', {
    t('<pre><code>'),
    i(1, 'here'),
    t('</code></pre>'),
  })
)

table.insert(
  html_snippets,
  s('input', {
    t('<input type="'),
    i(1, ''),
    t('" />'),
  })
)

table.insert(
  html_snippets,
  s('form', {
    t('<form>'),
    i(1, ''),
    t('</form>'),
  })
)

table.insert(
  html_snippets,
  s('button', {
    t('<button>'),
    i(1, ''),
    t('</button>'),
  })
)

table.insert(
  html_snippets,
  s('h1', {
    t('<h1>'),
    i(1, ''),
    t('</h1>'),
  })
)

table.insert(
  html_snippets,
  s('h2', {
    t('<h2>'),
    i(1, ''),
    t('</h2>'),
  })
)

table.insert(
  html_snippets,
  s('h3', {
    t('<h3>'),
    i(1, ''),
    t('</h3>'),
  })
)

table.insert(
  html_snippets,
  s('h4', {
    t('<h4>'),
    i(1, ''),
    t('</h4>'),
  })
)

table.insert(
  html_snippets,
  s('h5', {
    t('<h5>'),
    i(1, ''),
    t('</h5>'),
  })
)

table.insert(
  html_snippets,
  s('table', {
    t('<table>'),
    i(1, '#:here'),
    t('</table>'),
  })
)

table.insert(
  html_snippets,
  s('thead', {
    t('<thead>'),
    i(1, '#:here'),
    t('</thead>'),
  })
)

table.insert(
  html_snippets,
  s('tr', {
    t('<tr>'),
    i(1, '#:here'),
    t('</tr>'),
  })
)

table.insert(
  html_snippets,
  s('th', {
    t('<th>'),
    i(1, '#:here'),
    t('</th>'),
  })
)

table.insert(
  html_snippets,
  s('tbody', {
    t('<tbody>'),
    i(1, '#:here'),
    t('</tbody>'),
  })
)

table.insert(
  html_snippets,
  s('td', {
    t('<td>'),
    i(1, '#:here'),
    t('</td>'),
  })
)

table.insert(
  html_snippets,
  s('select', {
    t('<select>'),
    i(1, '#:here'),
    t('</select>'),
  })
)

table.insert(
  html_snippets,
  s('option', {
    t('<option>'),
    i(1, '#:here'),
    t('</option>'),
  })
)

table.insert(
  html_snippets,
  s('details', {
    t('<details>'),
    i(1, 'details'),
    t('</details>'),
  })
)

table.insert(
  html_snippets,
  s('summary', {
    t('<summary>'),
    i(1, 'summary'),
    t('</summary>'),
  })
)

table.insert(
  html_snippets,
  s('nav', {
    t('<nav>'),
    i(1, '#:here'),
    t('</nav>'),
  })
)

-- Templates
table.insert(
  html_snippets,
  s('script', {
    t('<script type="text/javascript">'),
    i(1, 'TARGET'),
    t('</script>'),
  })
)

table.insert(
  html_snippets,
  s('script_load', {
    t('<script src="'),
    i(1, 'TARGET'),
    t('"></script>'),
  })
)

vim.list_extend(
  html_snippets,
  sm({ 'link', 'css_load' }, {
    t('<link rel="'),
    i(1, 'stylesheet'),
    t('" href="'),
    i(2, 'index.css'),
    t('">'),
  })
)

table.insert(
  html_snippets,
  s('ahref', {
    t('<a href="'),
    i(1, ''),
    t('">'),
    i(2, 'TARGET'),
    t('</a>'),
  })
)

table.insert(
  html_snippets,
  s('aname', {
    t('<a name="'),
    i(1, ''),
    t('">'),
    i(2, 'TARGET'),
    t('</a>'),
  })
)

vim.list_extend(
  html_snippets,
  sm(
    { 'folding', 'details_summary' },
    fmt(
      [[
<details>
<summary>{}</summary> <!-- {{{{ -->
{}
<!-- }}}} -->
</details>]],
      {
        i(1, 'summary'),
        i(2, ''),
      }
    )
  )
)

vim.list_extend(
  html_snippets,
  sm(
    { 'folding_without_folding', 'details_summary_without_folding' },
    fmt(
      [[
<details>
<summary>{}</summary>
{}
</details>]],
      {
        i(1, 'summary'),
        i(2, ''),
      }
    )
  )
)

vim.list_extend(
  html_snippets,
  sm({ 'revealjs_speaker_notes', 'speaker_notes' }, {
    t('<aside class="notes">'),
    i(1, 'here'),
    t('</aside>'),
  })
)

table.insert(
  html_snippets,
  s('tag_close', {
    t('</'),
    i(1, 'name'),
    t('>'),
  })
)

table.insert(html_snippets, s('div_close', t('</div>')))

-- Others
table.insert(html_snippets, s('html_half_space', t('&ensp;')))

table.insert(html_snippets, s('html_large_space', t('&emsp;')))

table.insert(html_snippets, s('html_small_space', t('&thinsp;')))

return { snippets = html_snippets, autosnippets = {} }
