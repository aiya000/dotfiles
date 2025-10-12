local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node

return list.concat(
  sm(
    { 'define_module', 'module' },
    fmt([[
      local M = {{}}
      return M
    ]], {})
  ),

  sm(
    { 'import', 'imp' },
    fmt([[local {name} = require('{module}')]], {
      name = i(1, 'name'),
      module = i(2, 'module'),
    })
  ),

  sm(
    { 'class', 'cla' },
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

  -- See `:h vim.list_extend()` for about another arguments.
  -- Consider define below fucntion when you cannot use `vim`. {{{
  -- ```lua
  -----Simular to `vim.list_extend()`, but can take multiple lists (varargs).
  -----
  -----@generic T
  -----@param ... T
  -----@return T[]
  -----
  -----Example:
  -----```lua
  -----concat(
  -----  { 1, 2, 3 },
  -----  { 4, 5 },
  -----  { 6 }
  -----) -- { 1, 2, 3, 4, 5, 6 }
  -----```
  -- function concat(...)
  --   local result = {}
  --   for _, xs in ipairs({ ... }) do
  --     for _, x in ipairs(xs) do
  --       table.insert(result, x)
  --     end
  --   end
  --   return result
  -- end
  -- ```
  -- }}}
  sm(
    { 'list_concat', 'concat' },
    fmt([[vim.fn.extendnew({xs}, {ys})]], {
      xs = i(1, 'xs'),
      ys = i(2, 'ys'),
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
      'poi',
      fmt(
        [[vim.notify('poi: ' .. vim.inspect({}), vim.log.levels.INFO)]],
        {
          i(1, ''),
        }
      )
    ),

    s('autocmd',
      fmt([[
        vim.api.nvim_create_autocmd({event_or_events}, {{
          group = vim.api.nvim_create_augroup('{group_name}', {{ clear = true }}),
          callback = {callback},
        }})
      ]], {
        event_or_events = i(1, 'event_or_events'),
        group_name = i(2, 'group_name'),
        callback = i(3, 'callback'),
      })
    ),

    s('augroup',
      fmt([[vim.api.nvim_create_augroup('{group_name}', {{ clear = true }})]], {
        group_name = i(1, 'group_name'),
      })
    ),

    s('autocmd_without_group',
      fmt([[
        vim.api.nvim_create_autocmd({event_or_events}, {{
          callback = {callback},
        }})
      ]], {
        event_or_events = i(1, 'event_or_events'),
        callback = i(2, 'callback'),
      })
    ),
  }
)
