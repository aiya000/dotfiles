local list = require('utils.list')
local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require('luasnip.extras.fmt').fmt

-- Helper function to create snippets with multiple triggers
local function sm(triggers, nodes)
  local snippets = {}
  for _, trigger in ipairs(triggers) do
    table.insert(snippets, s(trigger, nodes))
  end
  return snippets
end

local zenn_snippets = {}

vim.list_extend(zenn_snippets, sm({"details", "zenn_details", "zenn_folding"}, fmt([[
:::details

{}

:::]], {
  i(1, "")
})))

vim.list_extend(zenn_snippets, sm({"message", "zenn_message", "zenn_info"}, fmt([[
:::message

{}

:::]], {
  i(1, "")
})))

return { snippets = zenn_snippets, autosnippets = {} }