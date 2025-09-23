local list = require('utils.list')
local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

-- Helper function to create snippets with multiple triggers
local function sm(triggers, nodes)
  local snippets = {}
  for _, trigger in ipairs(triggers) do
    table.insert(snippets, s(trigger, nodes))
  end
  return snippets
end

local markdown_pp_snippets = {}

vim.list_extend(
  markdown_pp_snippets,
  sm({ 'markdown_pp_include', 'include' }, {
    t('!INCLUDE "'),
    i(1, 'file-name.mdpp'),
    t('"'),
  })
)

return { snippets = markdown_pp_snippets, autosnippets = {} }
