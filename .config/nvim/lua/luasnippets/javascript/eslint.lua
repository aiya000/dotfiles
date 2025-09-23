local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node

return list.concat(
  sm(
    { 'eslint_disable_all', 'eslint_disable_file' },
    fmt(
      [[
    /* eslint-disable */
  ]],
      {}
    )
  ),

  {
    s(
      'eslint_disable_lines',
      fmt(
        [[
      /* eslint-disable {name} */
      {}
      /* eslint-enable {name} */
    ]],
        {
          name = i(1, 'name'),
          i(2, ''),
        }
      )
    ),

    s(
      'eslint_disable',
      fmt(
        [[
      /* eslint {name}: 0 */
    ]],
        {
          name = i(1, 'name'),
        }
      )
    ),

    s(
      'eslint_enable_warn',
      fmt(
        [[
      /* eslint {name}: 1 */
    ]],
        {
          name = i(1, 'name'),
        }
      )
    ),

    s(
      'eslint_enable_error',
      fmt(
        [[
      /* eslint {name}: 2 */
    ]],
        {
          name = i(1, 'name'),
        }
      )
    ),

    s(
      'eslint_ignore_next',
      fmt(
        [[
      // eslint-disable-next-line {}
    ]],
        {
          i(1, ''),
        }
      )
    ),
  },

  sm(
    { 'eslint_ignore_previous', 'eslint_ignore' },
    fmt(
      [[
    // eslint-disable-line {}
  ]],
      {
        i(1, ''),
      }
    )
  ),

  {
    s(
      'eslint_ignore_no_unused_vars',
      fmt(
        [[
      // eslint-disable-line no-unused-vars
    ]],
        {}
      )
    ),

    s(
      'eslint_ignore_no_unused_expressions',
      fmt(
        [[
      // eslint-disable-line no-unused-expressions
    ]],
        {}
      )
    ),

    s(
      'eslint_ignore_typescript_eslint_explicit_module_boundary_types',
      fmt(
        [[
      // eslint-disable-line @typescript-eslint/explicit-module-boundary-types
    ]],
        {}
      )
    ),

    s(
      'eslint_ignore_typescript_eslint_no_explicit_any',
      fmt(
        [[
      // eslint-disable-line @typescript-eslint/no-explicit-any
    ]],
        {}
      )
    ),
  },

  sm(
    { 'eslint_ignore_next_any', 'eslint_ignore_for_type_check_function' },
    fmt(
      [[
    // eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types, @typescript-eslint/no-explicit-any
  ]],
      {}
    )
  )
)
