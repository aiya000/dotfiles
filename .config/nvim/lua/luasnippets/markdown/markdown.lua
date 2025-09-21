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

-- Syntax

local markdown_snippets = {}

vim.list_extend(markdown_snippets, sm({"img", "image"}, {
  t("![]("), i(1, "here"), t(")")
}))

vim.list_extend(markdown_snippets, sm({"check", "ch"}, {
  t("- [ ] "), i(1, "")
}))

table.insert(markdown_snippets, s("checked", {
  t("- [x] "), i(1, "")
}))

table.insert(markdown_snippets, s("bar", t("- - - - -")))

vim.list_extend(markdown_snippets, sm({"block", "bl"}, fmt([[
```{}
{}
```]], {
    i(1, "#:type"),
    i(2, "")
  })))

table.insert(markdown_snippets, s("link", {
  t("["), i(1, "visible_text"), t("]("), i(2, "URL"), t(")"), i(3, "")
}))

vim.list_extend(markdown_snippets, sm({"footnote_reference", "fn"}, {
  t("[^"), i(1, "name"), t("]")
}))

table.insert(markdown_snippets, s("footnote", {
  t("[^"), i(1, "name"), t("]:")
}))

-- Emoji
vim.list_extend(markdown_snippets, sm({"sparkles", "p"}, t(":sparkles:")))

table.insert(markdown_snippets, s("up", t(":up:")))

table.insert(markdown_snippets, s("tada", t(":tada:")))

table.insert(markdown_snippets, s("bug", t(":bug:")))

table.insert(markdown_snippets, s("recycle", t(":recycle:")))

table.insert(markdown_snippets, s("niconiconi", t("🤟🙄🤟")))

-- Others
vim.list_extend(markdown_snippets, sm({"id", "anchor"}, {
  t('<a id="'), i(1, "section_name"), t('">')
}))

vim.list_extend(markdown_snippets, sm({"link_reference", "ref"}, {
  t("["), i(1, "visible_text"), t("](#"), i(2, "section_name"), t(")"), i(3, "")
}))

return { snippets = markdown_snippets, autosnippets = {} }