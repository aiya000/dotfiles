local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local i = ls.insert_node

return list.concat(
  sm(
    { 'luasnip_snippet', 'luasnip_s' },
    fmt(([[
      s('{name}',
        fmt(%s
          {here}
        %s, {{
          {i_nodes},
        }})
      ),
    ]]):format('[[', ']]'), {
      name = i(1, 'name'),
      here = i(2, 'here'),
      i_nodes = i(3, 'i_nodes'),
    })
  ),
  sm(
    { 'luasnip_snippet_multiple_triggers', 'luasnip_sm' },
    fmt(([[
      sm({{'{name1}', '{name2}'}},
        fmt(%s
          {here}
        %s, {{
          {i_nodes},
        }})
      ),
    ]]):format('[[', ']]'), {
      name1 = i(1, 'name1'),
      name2 = i(2, 'name2'),
      here = i(3, 'here'),
      i_nodes = i(4, 'i_nodes'),
    })
  )
)
