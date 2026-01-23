local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local sm = require('utils.luasnip').sm

local i = ls.insert_node
local s = ls.snippet

return list.concat(
  {
    s(
      'poi',
      fmt(
        [[
        vim.notify('poi: ' .. vim.inspect({}), vim.log.levels.INFO)
      ]],
        {
          i(1, ''),
        }
      )
    ),

    s(
      'notify',
      fmt(
        [[
        vim.notify({}, vim.log.levels.INFO)
      ]],
        {
          i(1, ''),
        }
      )
    ),

    s(
      'notify_warn',
      fmt(
        [[
        vim.notify({}, vim.log.levels.WARN)
      ]],
        {
          i(1, ''),
        }
      )
    ),

    s(
      'notify_error',
      fmt(
        [[
        vim.notify({}, vim.log.levels.ERROR)
      ]],
        {
          i(1, ''),
        }
      )
    ),

    s(
      'echo',
      fmt([[vim.api.nvim_echo({}, false, {{}})]], {
        i(1, ''),
      })
    ),

    s(
      'echomsg',
      fmt([[vim.api.nvim_echo({}, true, {{}})]], {
        i(1, ''),
      })
    ),

    s(
      'echo_warn',
      fmt([[vim.api.nvim_echo({{{}, 'WarningMsg'}}, false, {{}})]], {
        i(1, ''),
      })
    ),

    s(
      'echo_error',
      fmt([[vim.api.nvim_echo({{{}, 'ErrorMsg'}}, false, {{}})]], {
        i(1, ''),
      })
    ),

    s(
      'keymap',
      fmt(
        [[
        vim.keymap.set('{mode}', '{key}', {key_or_cmd}{, opts})
      ]],
        {
          mode = i(1, 'mode'),
          key = i(2, 'key'),
          key_or_cmd = i(3, 'key_or_cmd'),
          [', opts'] = i(4, ', opts'),
        }
      )
    ),

    s(
      'augroup',
      fmt([[vim.api.nvim_create_augroup('{group_name}', {{ clear = true }})]], {
        group_name = i(1, 'group_name'),
      })
    ),

    s(
      'autocmd',
      fmt(
        [[
        vim.api.nvim_create_autocmd({event_or_events}, {{
          group = vim.api.nvim_create_augroup('{group_name}', {{ clear = true }}),
          callback = {callback},
        }})
      ]],
        {
          event_or_events = i(1, 'event_or_events'),
          group_name = i(2, 'group_name'),
          callback = i(3, 'callback'),
        }
      )
    ),
  },

  sm(
    { 'home', 'stdpath_config' },
    fmt(
      [[
    vim.fn.stdpath('config')
  ]],
      {}
    )
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
  )
)
