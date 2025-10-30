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
      'define_module',
      fmt([[
        local M = {{}}
        return M
      ]], {})
    ),
  },

  sm(
    { 'import', 'imp' },
    fmt([[local {name} = require('{module}')]], {
      module = i(1, 'module'),
      name = i(2, 'name'),
    })
  ),

  sm(
    { 'define_class' },
    fmt([[
      ---@class {ClassName}
      local {ClassName} = {{}}
      {ClassName}.__index = {ClassName}

      ---@return {ClassName}
      function {ClassName}.new({constructor_args})
        local self = setmetatable({{}}, {ClassName})
        self.{arg} = {arg}
        return self
      end
    ]], {
      ClassName = i(1, 'ClassName'),
      constructor_args = i(2, 'constructor_args'),
      arg = i(3, 'arg'),
    })
  ),

  -- `table.concat()`がjoinで、`extendnew()`がconcatなの、紛らわしい
  sm(
    { 'list_join', 'join' },
    fmt([[table.concat({list}, '{sep}')]], {
      list = i(1, 'list'),
      sep = i(2, 'sep'),
    })
  ),

  {
    s(
      'list_push',
      fmt([[table.insert({list}, {})]], {
        list = i(1, 'list'),
        i(2, ''),
      })
    ),

    s(
      'todo',
      fmt(
        [[error('TODO: Not Implemented Yet ({function_name})')]],
        {
          function_name = i(1, 'function_name'),
        }
      )
    ),

    s(
      'try',
      fmt(
        [[
          local ok, result = pcall({f})
          if not ok then
            {}
          end
        ]],
        {
          f = i(1, 'f'),
          i(2, ''),
        }
      )
    ),
  }
)
