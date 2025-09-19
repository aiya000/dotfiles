-- HTML-like syntax snippets for neosnippet
local ls = require("luasnip")
local s = ls.snippet
local sn = ls.snippet_node
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local c = ls.choice_node
local d = ls.dynamic_node
local r = ls.restore_node
local l = require("luasnip.extras").lambda
local rep = require("luasnip.extras").rep
local p = require("luasnip.extras").partial
local m = require("luasnip.extras").match
local n = require("luasnip.extras").nonempty
local dl = require("luasnip.extras").dynamic_lambda
local fmt = require("luasnip.extras.fmt").fmt
local fmta = require("luasnip.extras.fmt").fmta
local types = require("luasnip.util.types")
local conds = require("luasnip.extras.expand_conditions")

return {
  s({trig = "snippet_template_xml_attr", namr = "snippet_template_xml_attr", dscr = "XML attribute snippet template"}, fmt([[
snippet {name}
abbr {name}="${{0:here}}"
  {name}="${{0:here}}"]], {
    name = i(1, "#:snippetName"),
  })),

  s({trig = "snippet_template_xml_tag", namr = "snippet_template_xml_tag", dscr = "XML self-closing tag snippet template"}, fmt([[
snippet {name}
abbr <{name}/>
  <{name}/>]], {
    name = i(1, "#:snippetName"),
  })),

  s({trig = "snippet_template_xml_tag_surround", namr = "snippet_template_xml_tag_surround", dscr = "XML surrounding tag snippet template"}, fmt([[
snippet {name}
abbr <{name}>${{0:#:here}}</{name}>
  <{name}>${{0:#:here}}</{name}>]], {
    name = i(1, "#:snippetName"),
  })),
}