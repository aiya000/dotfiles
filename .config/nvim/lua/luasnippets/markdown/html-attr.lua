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

local html_attr_snippets = {}

vim.list_extend(html_attr_snippets, sm({"html_attribute", "attribute", "attr", "att"}, {
  i(1, "name"), t('="'), i(2, "here"), t('"')
}))

table.insert(html_attr_snippets, s("colspan", {
  t('colspan="'), i(1, "here"), t('"')
}))

vim.list_extend(html_attr_snippets, sm({"class", "cl", "cla", "html_class"}, {
  t('class="'), i(1, "name"), t('"')
}))

table.insert(html_attr_snippets, s("id", {
  t('id="'), i(1, "id"), t('"')
}))

table.insert(html_attr_snippets, s("height", {
  t('height="'), i(1, "64"), t('"')
}))

table.insert(html_attr_snippets, s("width", {
  t('width="'), i(1, "64"), t('"')
}))

table.insert(html_attr_snippets, s("name", {
  t('name="'), i(1, "here"), t('"')
}))

table.insert(html_attr_snippets, s("type", {
  t('type="'), i(1, "here"), t('"')
}))

table.insert(html_attr_snippets, s("rel", {
  t('rel="'), i(1, "here"), t('"')
}))

table.insert(html_attr_snippets, s("alt", {
  t('alt="'), i(1, "here"), t('"')
}))

table.insert(html_attr_snippets, s("href", {
  t('href="'), i(1, "here"), t('"')
}))

table.insert(html_attr_snippets, s("style", {
  t('style="'), i(1, "here"), t('"')
}))

-- Templates
vim.list_extend(html_attr_snippets, sm({"rel_noopener", "target_blank_secure_attr"}, t('rel="noopener"')))

return { snippets = html_attr_snippets, autosnippets = {} }