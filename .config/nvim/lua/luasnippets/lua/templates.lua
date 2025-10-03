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
    { 'define_print_table', 'define_print_object', 'define_print_dict' },
    fmt([[
      local function print_table(t)
        print('{{')
        for k, v in pairs(t) do
          print('  ' .. k, '=', v .. ',')
        end
        print('}}')
      end
    ]], {})
  ),

  sm(
    { 'print_table', 'prt', 'pt' },
    fmt([[{module}print_table({var})]], {
      module = i(1, 'module.'),
      var = i(2, 'var'),
    })
  ),

  {
    s(
      'define_pipe',
      fmt([[
        local function pipe(value)
          return {{
            value = value,
            let = function(self, f)
              return pipe(f(self.value))
            end,
            get = function(self)
              return self.value
            end
          }}
        end
      ]], {})
    ),
  },

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

  sm(
    { 'method', 'met' },
    fmt([[
      function {class}:{method_name}()
        {}
      end
    ]], {
      class = i(1, 'class'),
      method_name = i(2, 'method_name'),
      i(3, ''),
    })
  ),

  {
    s(
      'call_lambda',
      fmt([[
        (function {f}({args})
           {}
        end)({args})
      ]], {
        f = i(1, 'f'),
        args = i(2, 'args'),
        i(3, ''),
      })
    ),
  },

  {
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
