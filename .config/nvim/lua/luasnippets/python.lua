-- Import all Python snippet modules
local python_snippets = require('luasnippets.python.python')
local docstring_snippets = require('luasnippets.python.docstring')
local mypy_snippets = require('luasnippets.python.mypy')

-- Manually concatenate all snippets using vim.list_extend for LuaSnip compatibility
local all_snippets = {}
vim.list_extend(all_snippets, python_snippets.snippets)
vim.list_extend(all_snippets, docstring_snippets.snippets)
vim.list_extend(all_snippets, mypy_snippets.snippets)

return {
  snippets = all_snippets,
  autosnippets = {},
}
