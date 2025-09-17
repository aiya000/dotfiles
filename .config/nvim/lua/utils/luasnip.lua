local s = require('luasnip').snippet

local M = {}

---Same as `local s = require('luasnip').snippet`, but arguments are typed
---@param trigger string
---@param snip table --A result of either `require('luasnip.extras.fmt').fmt()` or `require('utils.luasnip').text_nodet()()`
---@param opts? table --?
---@return unknown --A result of `require('luasnip').snippet()`
function M.s(trigger, snip, opts)
  return s(trigger, snip, opts)
end

---Creates a snippet with multiple triggers (aliases)
---@param triggers string[]
---@param snip table --A result of `require('luasnip.extras.fmt').fmt()`
---@param opts? table --?
---@return unknown[] --A list of `require('luasnip').snippet()`
---
---Example:
---```lua:snippets/typescript.lua
---return vim.tbl_extend(
---  'force',
---  {},
---  sm({'fun', 'function'}, fmt([[
---    function {name}({args}) {{
---      {}
---    }}
---  ]], {
---    name = i(1, 'name'),
---    args = i(2, 'args'),
---    i(3, ''),
---  })),
---  {}
---)
---```
function M.snip_by_multiple_triggers(triggers, snip, opts)
  return vim.iter(ipairs(triggers))
    :map(function(_, trigger)
      -- Why deepcopy? See: https://www.reddit.com/r/neovim/comments/tzd135/regex_pattern_or_for_luasnip_lua_pattern_matching
      -- Non functional programming is really XXXXXX!
      return vim.deepcopy(M.s(trigger, snip, opts))
    end)
    :totable()
end

M.sm = M.snip_by_multiple_triggers

return M
