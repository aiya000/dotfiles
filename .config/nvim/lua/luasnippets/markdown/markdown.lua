local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local sm = require('utils.luasnip').sm

local i = ls.insert_node
local s = ls.snippet
local t = ls.text_node

return list.concat(
  sm({ 'img', 'image' }, fmt('![]({})', { i(1, 'here') })),
  sm({ 'check', 'ch' }, fmt('- [ ] {}', { i(1, '') })),

  {
    s('checked', fmt('- [x] {}', { i(1, '') })),
    s('bar', t('- - -')),
    s('barr', t('- - - - -')),
  },

  sm(
    { 'block', 'bl' },
    fmt([[
      ```{}
      {}
      ```
    ]], {
      i(1, ''),
      i(2, ''),
    })
  ),

  {
    s('link', fmt('[{}]({})', {
      i(1, ''),
      i(2, ''),
    })),
  },

  sm({
    'footnote_reference', 'fn' },
    fmt('[^{name}]',
    { name = i(1, 'name') })
  ),

  {
    s('footnote', fmt('[^{name}]: {}', { name = i(1, 'name'), i(2, '') })),
    s('niconiconi', t('🤟🙄🤟')),
  },

  sm({ 'id', 'anchor' }, fmt('<a id="{}">', { i(1, 'section_name') }))
)
