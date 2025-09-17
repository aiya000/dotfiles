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

local markdown_snippets = list.concat({
  sm({"img", "image"}, {
    t("![]("), i(1, "here"), t(")")
  }),

  sm({"check", "ch"}, {
    t("- [ ] "), i(1, "")
  }),

  s("checked", {
    t("- [x] "), i(1, "")
  }),

  s("bar", t("- - - - -")),

  sm({"block", "bl"}, fmt([[
```{}
{}
```]], {
    i(1, "#:type"),
    i(2, "")
  })),

  s("link", {
    t("["), i(1, "visible_text"), t("]("), i(2, "URL"), t(")"), i(3, "")
  }),

  sm({"footnote_reference", "fn"}, {
    t("[^"), i(1, "name"), t("]")
  }),

  s("footnote", {
    t("[^"), i(1, "name"), t("]:")
  }),

  -- Emoji
  sm({"sparkles", "p"}, t(":sparkles:")),

  s("up", t(":up:")),

  s("tada", t(":tada:")),

  s("bug", t(":bug:")),

  s("recycle", t(":recycle:")),

  s("niconiconi", t("🤟🙄🤟")),

  -- Others
  sm({"id", "anchor"}, {
    t('<a id="'), i(1, "section_name"), t('">')
  }),

  sm({"link_reference", "ref"}, {
    t("["), i(1, "visible_text"), t("](#"), i(2, "section_name"), t(")"), i(3, "")
  })
})

return markdown_snippets