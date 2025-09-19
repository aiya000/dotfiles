local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node

return list.concat(
  sm({'luasnip_snippet', 'luasnip_s'},
    fmt([[
      s('{name}',
        fmt(%s
          {}
        %s, {{
          {i_nodes},
        }})
      ),
    ]])
    :format('[[', ']]'),
    {
      name = i(1, 'name'),
      i_nodes = i(2, 'i_nodes'),
      i(3, ''),
    }
  ),
  sm({'luasnip_snippet_multiple_triggers', 'luasnip_sm'},
    fmt([[
      s({{'{name1}', '{name2}'}},
        fmt(%s
          {}
        %s, {{
          {i_nodes},
        }})
      ),
    ]])
    :format('[[', ']]'),
    {
      name1 = i(1, 'name1'),
      name2 = i(2, 'name2'),
      i_nodes = i(3, 'i_nodes'),
      i(4, ''),
    }
  )
)
