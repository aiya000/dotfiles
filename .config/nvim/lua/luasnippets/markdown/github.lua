local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local i = ls.insert_node
local s = ls.snippet
local sm = utils.sm
local t = ls.text_node

return list.concat(
  sm(
    { 'alert_tip', 'tip' },
    fmt([[
      > [!TIP]
      > {}
    ]], {
      i(1, ''),
    })
  ),

  sm(
    { 'alert_note', 'note' },
    fmt([[
      > [!NOTE]
      > {}
    ]], {
      i(1, ''),
    })
  ),

  sm(
    { 'alert_important', 'important' },
    fmt([[
      > [!IMPORTANT]
      > {}
    ]], {
      i(1, ''),
    })
  ),

  sm(
    { 'alert_warning', 'warning' },
    fmt([[
      > [!WARNING]
      > {}
    ]], {
      i(1, ''),
    })
  ),

  sm(
    { 'alert_caution', 'caution' },
    fmt([[
      > [!caution]
      > {}
    ]], {
      i(1, ''),
    })
  )
)
