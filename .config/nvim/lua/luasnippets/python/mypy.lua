local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node

-- Convert to proper array structure for LuaSnip compatibility
local mypy_snippets = {}

table.insert(mypy_snippets, s('mypy_ignore_this_line', t('# type: ignore')))
table.insert(mypy_snippets, s('mypy_ignore_all', t('# mypy: ignore')))

return { snippets = mypy_snippets, autosnippets = {} }
