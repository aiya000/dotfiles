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

-- Syntaxes

local cs_snippets = list.concat({
  s("if", fmt([[
if ({}) {{{}}}]], {
    i(1, "#:cond"),
    i(2, "#:here")
  })),

  s("else", fmt([[
else {{{}}}]], {
    i(1, "#:here")
  })),

  sm({"foreach", "for"}, fmt([[
foreach (var {} in {}) {{{}}}]], {
    i(1, "x"),
    i(2, "xs"),
    i(3, "here")
  })),

  sm({"for_traditional", "fort"}, fmt([[
for (var {} = {}; {}; {}) {{{}}}]], {
    i(1, "i"),
    i(2, "0"),
    i(3, "condition"),
    i(4, "i++"),
    i(5, "here")
  })),

  s("switch", fmt([[
switch ({}) {{{}}}]], {
    i(1, "#:var"),
    i(2, "#:here")
  })),

  s("try", fmt([[
try {{{}}}]], {
    i(1, "#:here")
  })),

  s("catch", fmt([[
catch ({} e) {{{}}}]], {
    i(1, "Exception"),
    i(2, "#:here")
  })),

  s("finally", fmt([[
finally {{{}}}]], {
    i(1, "#:here")
  })),

  sm({"using_resources", "using"}, fmt([[
using ({}) {{{}}}]], {
    i(1, "#:var resource"),
    i(2, "#:here")
  })),

  s("get", fmt([[
get {{{}}}]], {
    i(1, "#:here")
  })),

  s("set", fmt([[
set {{{}}}]], {
    i(1, "#:here")
  })),

  sm({"index", "class_index", "this_index"}, {
    i(1, "Type"), t(" this["), i(2, "int index"), t("] {"), i(3, "getter_and_setter"), t("}")
  }),

  s("namespace", fmt([[
namespace {} {{{}}}]], {
    i(1, "#:Name"),
    i(2, "#:here")
  })),

  sm({"class", "cla"}, fmt([[
class {} {{{}}}]], {
    i(1, "#:Name"),
    i(2, "#:here")
  })),

  s("constructor", {
    -- Using vim function to get filename without extension
    t("`expand('%:t:r')`("), i(1, "#:Parameter"), t(") {"), i(2, ""), t("}")
  }),

  sm({"method", "met"}, {
    i(1, "void"), t(" "), i(2, "Name"), t("("), i(3, "#:args"), t(") "), i(4, "")
  }),

  sm({"public_method", "pubmet"}, {
    t("public "), i(1, "void"), t(" "), i(2, "Name"), t("("), i(3, "#:args"), t(") "), i(4, "")
  }),

  sm({"private_method", "primet"}, {
    t("private "), i(1, "void"), t(" "), i(2, "Name"), t("("), i(3, "#:args"), t(") "), i(4, "")
  }),

  sm({"public_static_method", "public_function", "pubfun"}, {
    t("public static "), i(1, "void"), t(" "), i(2, "Name"), t("("), i(3, "#:args"), t(") "), i(4, "")
  }),

  sm({"private_static_method", "private_function", "prifun"}, {
    t("private static "), i(1, "void"), t(" "), i(2, "Name"), t("("), i(3, "#:args"), t(") "), i(4, "")
  }),

  sm({"import", "imp"}, {
    t("using "), i(1, ""), t(";")
  }),

  -- Expressions
  sm({"println", "pr"}, {
    t("Console.WriteLine("), i(1, ""), t(")")
  }),

  s("readln", t("Console.ReadLine()")),

  sm({"debug_log", "d", "log"}, {
    t("Debug.Log("), i(1, "#:here"), t(")")
  }),

  sm({"debug_log_error", "e", "log_error"}, {
    t("Debug.LogError("), i(1, "#:here"), t(")")
  }),

  s("list", {
    t("new [] {"), i(1, "#:here"), t("}")
  }),

  -- Document comments
  s("summary", {
    t("<summary>"), i(1, ""), t("</summary>")
  }),

  s("see", {
    t('<see cref="'), i(1, "#:some_object"), t('"/>')
  }),

  s("seealso", {
    t('<seealso cref="'), i(1, "#:some_object"), t('"/>')
  }),

  s("c", {
    t("<c>"), i(1, ""), t("</c>")
  }),

  s("example", {
    t("<example>"), i(1, ""), t("</example>")
  }),

  s("code", {
    t("<code>"), i(1, ""), t("</code>")
  }),

  s("doc", fmt([[
/// <summary>
/// {}
/// </summary>]], {
    i(1, "")
  }))
})

return cs_snippets