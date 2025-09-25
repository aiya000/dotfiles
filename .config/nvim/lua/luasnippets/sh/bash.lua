local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = utils.s
local i = ls.insert_node
local t = ls.text_node

return list.concat(
  -- function definition
  sm(
    { 'function', 'func', 'fun' },
    fmt(
      [[
function {} () {{
    {}
}}]],
      {
        i(1, '#:name'),
        i(0, 'TARGET'),
      }
    )
  ),

  -- test operations
  {
    s(
      'test',
      fmt('[[ {} ]]', {
        i(0),
      })
    ),

    s(
      'test_regex',
      fmt('[[ {} =~ {} ]]', {
        i(1),
        i(0),
      })
    ),

    -- variable replacement operations
    s(
      'var_replace_only_first',
      fmt([[${{{1}/{2}/{3}}}{4}]], {
        i(1, 'varname'),
        i(2, 'pattern_str'),
        i(3, 'replaced'),
        i(0),
      })
    ),

    s(
      'var_replace_all',
      fmt([[${{{}//{}/${{{}}}}}{}]], {
        i(1, 'varname'),
        i(2, 'pattern_str'),
        i(3, 'replaced'),
        i(0),
      })
    ),

    s(
      'var_replace_head',
      fmt([[${{{}/#{}/${{{}}}}}{}]], {
        i(1, 'varname'),
        i(2, 'pattern_str'),
        i(3, 'replaced'),
        i(0),
      })
    ),

    s(
      'var_replace_tail',
      fmt([[${{{}/%{}/${{{}}}}}{}]], {
        i(1, 'varname'),
        i(2, 'pattern_str'),
        i(3, 'replaced'),
        i(0),
      })
    ),

    s(
      'var_or_default',
      fmt([[${{{1}:-{2}}}{}]], {
        i(1, 'varname'),
        i(2, 'default_value'),
        i(0),
      })
    ),
  },

  -- array slicing
  sm(
    { 'array_slice', 'slice' },
    fmt([[${{{1}[@]:{2}:{3}}}{}]], {
      i(1, 'array'),
      i(2, 'start'),
      i(3, 'end'),
      i(0),
    })
  ),

  {
    s(
      'assign_sliced_array',
      fmt([[{}=("${{{2}[@]:{3}:{4}}}"){}]], {
        i(1, 'new_array'),
        i(2, 'old_array'),
        i(3, 'start'),
        i(4, 'end'),
        i(0),
      })
    ),
  }
)
