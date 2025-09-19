local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node

return sm({'vim_system', 'neovim_system', 'system'}, fmt([[
  vim.system({{'{command}'}}, {{ text = true }})
]], {
  command = i(1, 'command'),
}))