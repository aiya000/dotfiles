local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node

return list.concat(
  {
    s(
      'zod_interface',
      fmt(
        [[
      const {name} = z.object({{
        {}
      }})
    ]],
        {
          name = i(1, 'name'),
          i(2, ''),
        }
      )
    ),
  },

  sm(
    { 'zod_type', 'zod_infer' },
    fmt(
      [[
    type {Name} = z.infer<typeof {name}>{};
  ]],
      {
        Name = i(1, 'Name'),
        name = i(2, 'name'),
        i(3, ''),
      }
    )
  )
)
