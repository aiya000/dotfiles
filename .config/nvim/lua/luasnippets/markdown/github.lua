local fmt = require('luasnip.extras.fmt').fmt
local i = require('luasnip').insert_node
local list = require('utils.list')
local sm = require('utils.luasnip').sm

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
