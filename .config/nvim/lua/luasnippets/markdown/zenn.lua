local fmt = require('luasnip.extras.fmt').fmt
local i = require('luasnip').insert_node
local list = require('utils.list')
local sm = require('utils.luasnip').sm

return list.concat(
  sm(
    { 'details', 'zenn_details', 'zenn_folding' },
    fmt([[
      :::details

      {}

      :::
    ]], {
      i(1, ''),
    })
  ),

  sm(
    { 'message', 'zenn_message', 'zenn_info' },
    fmt([[
      :::message

      {}

      :::
    ]], {
      i(1, ''),
    })
  )
)
