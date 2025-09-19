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

local cpp_snippets = list.concat({
  s("template", {
    t("template <"), i(1, "typename"), t(" "), i(2, "T"), t(">")
  }),

  s("for", fmt([[
for ({} {} : {}) {{
    {}
}}]], {
    i(1, "auto&&"),
    i(2, "x"),
    i(3, "xs"),
    i(4, "TARGET")
  })),

  s("for_with_index", fmt([[
for ({}; {}; {}) {{
    {}
}}]], {
    i(1, "var"),
    i(2, "condition"),
    i(3, "effect"),
    i(4, "TARGET")
  })),

  s("if", fmt([[
if ({}) {{
    {}
}}]], {
    i(1, "condition"),
    i(2, "")
  })),

  s("else", fmt([[
else {{
    {}
}}]], {
    i(1, "")
  })),

  s("switch", fmt([[
switch ({}) {{
{}
}}]], {
    i(1, "target"),
    i(2, "")
  })),

  s("while", fmt([[
while ({}) {{
    {}
}}]], {
    i(1, "condition"),
    i(2, "")
  })),

  s("try", fmt([[
try {{
    {}
}}]], {
    i(1, "")
  })),

  s("catch", fmt([[
catch ({}) {{
    {}
}}]], {
    i(1, "exception"),
    i(2, "")
  })),

  s("finally", fmt([[
finally {{
    {}
}}]], {
    i(1, "")
  })),

  s("namespace", fmt([[
namespace {} {{
    {}
}}]], {
    i(1, "foo::bar"),
    i(2, "")
  })),

  s("namespace_anonymous", fmt([[
namespace {{
    {}
}}]], {
    i(1, "")
  })),

  s("namespace_alias", {
    t("namespace "), i(1, "alias"), t(" = "), i(2, "")
  }),

  s("using", {
    t("using "), i(1, "")
  }),

  s("class", fmt([[
class {} {{
    {}
}};]], {
    i(1, "Name"),
    i(2, "")
  })),

  s("struct", fmt([[
struct {} {{
    {}
}};]], {
    i(1, "Name"),
    i(2, "")
  })),

  s("enum", fmt([[
enum {} {{
    {}
}};]], {
    i(1, "Name"),
    i(2, "")
  })),

  s("enum_class", fmt([[
enum class {} {{
    {}
}};]], {
    i(1, "Name"),
    i(2, "")
  })),

  sm({"lambda", "lam"}, {
    t("["), i(1, ""), t("]("), i(2, ""), t("){"), i(3, ""), t("}")
  }),

  s("noexcept", t("noexcept")),

  -- Macros
  sm({"include_library", "include"}, {
    t("#include <"), i(1, "path"), t(">")
  }),

  sm({"include_myown", "includeo"}, {
    t('#include "'), i(1, "path"), t('"')
  }),

  -- Expressions
  s("demangle", {
    t("abi::__cxa_demangle("), i(1, "#:var"), t(".name(), 0, 0, nullptr);")
  }),

  sm({"println", "pr"}, {
    t("std::cout << "), i(1, "something"), t(" << '\\n';")
  }),

  s("print", {
    t("std::cout << "), i(1, "something"), t(";")
  }),

  s("wprintln", {
    t("std::wcout << "), i(1, "something"), t(" << '\\n';")
  }),

  s("wprint", {
    t("std::wcout << "), i(1, "something"), t(";")
  }),

  -- Templates
  sm({"do_demangle", "lets_demangle", "demangle_template"}, fmt([[
#include <cxxabi.h> // copy and paste this to the top of your file
char* typeName = abi::__cxa_demangle(typeid({}).name(), 0, 0, nullptr);
{}
std::free(typeName);]], {
    i(1, "#:var"),
    i(2, "")
  })),

  s("catch_std_exception", fmt([[
catch (const std::exception& e) {{
    {}
}}]], {
    i(1, "")
  })),

  sm({"get_type_of_exception_ptr", "exception_ptr_type"}, fmt([[
std::cout << ({} ? {}.__cxa_exception_type()->name() : "null") << std::endl;]], {
    i(1, "p"),
    i(1, "")
  })),

  sm({"do_at_scope_end", "finally"}, fmt([[
struct {} {{
    std::function<void()> f;
    {}(std::function<void()> f) : f(f) {{}}
    ~{}() {{
        this->f();
    }}
}};]], {
    i(1, "Finally"),
    i(1, ""),
    i(1, "")
  })),

  s("main", {
    t("int main() {"), i(1, "here"), t("}")
  }),

  sm({"unique_resource", "scoped_resource", "scoped_guard", "finally"}, fmt([[
/*!
 * A 'finally' implementation (like N3949),
 * this doesn't have thread safeties.
 */
class unique_resource {{
    std::function<void()> finally;
public:
    unique_resource(const unique_resource&) = delete;
    explicit unique_resource(std::function<void()> finally) : finally(finally) {{}}
    ~unique_resource() {{
        this->finally();
    }}
}};]], {})),

  sm({"vector_filter", "filter"}, fmt([[
std::vector<{}> {};
std::copy_if({}.begin(), {}.end(), std::back_inserter({}), {});]], {
    i(1, "int"),
    i(2, "zs"),
    i(3, "xs"),
    i(3, ""),
    i(2, ""),
    i(4, "predicate")
  })),

  s("newtype_integral", {
    t("enum class "), i(1, "Name"), t(" : "), i(2, "int"), t(" {};")
  }),

  s("newtype_integral_use", {
    i(1, "Name"), t(" "), i(2, "x"), t(" = "), i(1, ""), t("{"), i(3, "10"), t("};")
  })
})

return cpp_snippets