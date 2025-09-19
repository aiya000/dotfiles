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

local html_attr_snippets = list.concat({
  sm({"html_attribute", "attribute", "attr", "att"}, {
    i(1, "name"), t('="'), i(2, "here"), t('"')
  }),

  s("colspan", {
    t('colspan="'), i(1, "here"), t('"')
  }),

  sm({"class", "cl", "cla", "html_class"}, {
    t('class="'), i(1, "name"), t('"')
  }),

  s("id", {
    t('id="'), i(1, "id"), t('"')
  }),

  s("height", {
    t('height="'), i(1, "64"), t('"')
  }),

  s("width", {
    t('width="'), i(1, "64"), t('"')
  }),

  s("name", {
    t('name="'), i(1, "here"), t('"')
  }),

  s("type", {
    t('type="'), i(1, "here"), t('"')
  }),

  s("rel", {
    t('rel="'), i(1, "here"), t('"')
  }),

  s("alt", {
    t('alt="'), i(1, "here"), t('"')
  }),

  s("href", {
    t('href="'), i(1, "here"), t('"')
  }),

  s("style", {
    t('style="'), i(1, "here"), t('"')
  }),

  -- Templates
  sm({"rel_noopener", "target_blank_secure_attr"}, t('rel="noopener"'))
})

return html_attr_snippets