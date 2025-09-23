local fmt = require('luasnip.extras.fmt').fmt
local ls = require('luasnip')

local s = ls.snippet
local i = ls.insert_node

return {
  s(
    'composable',
    fmt(
      [[
    export const use{Something} = () => {{
      return {{
          {}
      }}
    }}

    export type {Something}Composable = ReturnType<typeof use{Something}>
    export const {something}InjectionKey: InjectionKey<{Something}Composable>
      = Symbol('{something}')
  ]],
      {
        Something = i(1, 'Something'),
        i(2, ''),
        something = i(3, 'something'),
      }
    )
  ),
}
