local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

return list.concat(
  {
    s(
      'if',
      fmt(
        [[
      if {condition} then
        {}
      end
    ]],
        {
          condition = i(1, 'condition'),
          i(2, ''),
        }
      )
    ),
  },

  sm({ 'equal', 'eq' }, t('==')),
  sm({ 'not_equal', 'ne' }, t('~=')),

  {
    s(
      'for_in',
      fmt(
        [[
      for {x} in {xs} do
        {}
      end
    ]],
        {
          x = i(1, 'x'),
          xs = i(2, 'xs'),
          i(3, ''),
        }
      )
    ),
  },

  sm(
    { 'for_traditional', 'for_range' },
    fmt(
      [[
    for {i} = 1, #{xs} do
      {}
    end
  ]],
      {
        i = i(1, 'i'),
        xs = i(2, 'xs'),
        i(3, ''),
      }
    )
  ),

  sm(
    { 'foreach', 'for' },
    fmt(
      [[
    for _, {x} in ipairs({xs}) do
      {}
    end
  ]],
      {
        x = i(1, 'x'),
        xs = i(2, 'xs'),
        i(3, ''),
      }
    )
  ),

  sm(
    { 'function', 'fun' },
    fmt([[
      function {f}({})
        {}
      end
    ]], {
      f = i(1, 'f'),
      i(2, ''),
      i(3, ''),
    })
  ),

  sm(
    { 'local_function', 'lfun' },
    fmt([[
      local function {f}({})
        {}
      end
    ]], {
      f = i(1, 'f'),
      i(2, ''),
      i(3, ''),
    })
  ),

  sm(
    { 'function_in_module', 'mfun' },
    fmt([[
      function M.{f}({})
        {}
      end
    ]], {
      f = i(1, 'f'),
      i(2, ''),
      i(3, ''),
    })
  ),

  sm(
    { 'lambda', 'lam' },
    fmt([[function({}) {} end]], {
      i(1, ''),
      i(2, ''),
    })
  ),

  sm(
    { 'array_length', 'len' },
    fmt('#{array}', {
      array = i(1, 'array'),
    })
  ),

  sm(
    { 'multi_line_comment', 'comment', 'com' },
    fmt(([[
      --%s
      {}
      --%s
    ]]):format('[[', ']]'), {
      i(1, ''),
    })
  ),

  {
    s(
      'require',
      fmt("require('{}')", {
        i(1, ''),
      })
    ),
  },

  sm(
    { 'print', 'pr' },
    fmt('print({})', {
      i(1, ''),
    })
  ),

  sm(
    { 'error', 'panic', 'er', 'err' },
    fmt('error({})', {
      i(1, ''),
    })
  ),

  {
    s(
      'assert',
      fmt("assert({condition}, '{error_message}')", {
        condition = i(1, 'condition'),
        error_message = i(2, 'error_message'),
      })
    ),
  },

  sm(
    { 'conditional_operator', 'cond' },
    fmt('{condition} and {if_true} or {if_false}', {
      condition = i(1, 'condition'),
      if_true = i(2, 'if_true'),
      if_false = i(3, 'if_false'),
    })
  ),

  {
    s('nullish_operator', t('or')),
  }
)
