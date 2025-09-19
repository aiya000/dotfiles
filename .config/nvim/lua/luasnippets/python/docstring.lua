local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require("luasnip.extras.fmt").fmt

local function sm(trigger, nodes, aliases)
  local snippets = { s(trigger, nodes) }
  for _, alias in ipairs(aliases or {}) do
    table.insert(snippets, s(alias, nodes))
  end
  return snippets
end

return {
  s("doc", fmt('"""{}"""', { i(1, "#:here") })),

  sm("doc_attributes", t([[Attributes
----------]]), {"attrs"}),

  sm("doc_attribute",
    fmt([[{} : {}
    {}]], { i(1, "field_name"), i(2, "type"), i(3, "description") }),
    {"doc_attr", "attr", "doc_parameter", "doc_param", "param"}),

  sm("doc_methods", t([[Methods
-------]]), {"methods"}),

  s("doc_method",
    fmt([[{}({})
    {}]], { i(1, "method_name"), i(2, "#:args"), i(3, "description") })),

  sm("doc_parameters", t([[Parameters
----------]]), {"parameters", "params"}),
}