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
  -- Syntaxes
  s("import_single", fmt("import {}", { i(1, "module") })),

  sm("import_as", fmt("import {} as {}", { i(1, "module"), i(2, "alias") }), {"import_qualified", "imq"}),

  sm("from", fmt("from {} import {}", { i(1, "module"), i(2, "stuff") }), {"import", "imp"}),

  s("if", fmt("if {}:{}", { i(1), i(0) })),

  s("else", t("else:")),

  s("for", fmt("for {} in {}:{}", { i(1, "x"), i(2, "xs"), i(0) })),

  s("while", fmt("while {}:{}", { i(1, "cond"), i(0) })),

  sm("def", fmt("def {}({}) -> {}:{}", { i(1, "name"), i(2, "#:self"), i(3, "type"), i(0) }), {"fun"}),

  sm("class", fmt("class {}{}:{}", { i(1, "Name"), i(2, "#:(Super)"), i(0) }), {"cla"}),

  sm("conditional_operator", fmt("{} if {} else {}", { i(1, "value_if_true"), i(2, "cond"), i(3, "value_if_false") }), {"cond"}),

  sm("lambda", fmt("lambda {}: {}", { i(1, "args"), i(0) }), {"lam"}),

  s("raise", fmt("raise {}({})", { i(1, "Exception"), i(2, "msg") })),

  s("try", t("try:")),

  sm("except", fmt("except{}: {}", { i(1, "#: ErrorType"), i(0) }), {"catch", "handle"}),

  sm("list_comprehension", fmt("[{} for {} in {}]", { i(3, "result"), i(1, "var"), i(2, "source") }), {"list"}),

  -- Templates
  sm("print", fmt("print({})", { i(0) }), {"pr"}),

  sm("__init__",
    fmt([[def __init__(self{}) -> None:
    {}]], { i(1, "#:, x"), i(0) }),
    {"init"}),

  sm("__post_init__",
    fmt([[def __post_init__(self{}) -> None:
    {}]], { i(1, "#:, x"), i(0) }),
    {"post_init"}),

  sm("__str__",
    fmt([[def __str__(self) -> str:
    {}]], { i(0) }),
    {"__str"}),

  s("if_name_is_main",
    fmt([[if __name__ == '__main__':
    {}]], { i(0) })),
}