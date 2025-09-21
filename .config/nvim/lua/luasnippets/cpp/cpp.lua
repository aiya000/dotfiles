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

local cpp_snippets = {}

table.insert(cpp_snippets, s("template", {
  t("template <"), i(1, "typename"), t(" "), i(2, "T"), t(">")
}))

table.insert(cpp_snippets, s("for", fmt([[
for ({} {} : {}) {{
    {}
}}]], {
  i(1, "auto&&"),
  i(2, "x"),
  i(3, "xs"),
  i(4, "TARGET")
})))

table.insert(cpp_snippets, s("for_with_index", fmt([[
for ({}; {}; {}) {{
    {}
}}]], {
  i(1, "var"),
  i(2, "condition"),
  i(3, "effect"),
  i(4, "TARGET")
})))

table.insert(cpp_snippets, s("if", fmt([[
if ({}) {{
    {}
}}]], {
  i(1, "condition"),
  i(2, "")
})))

table.insert(cpp_snippets, s("else", fmt([[
else {{
    {}
}}]], {
  i(1, "")
})))

table.insert(cpp_snippets, s("switch", fmt([[
switch ({}) {{
{}
}}]], {
  i(1, "target"),
  i(2, "")
})))

table.insert(cpp_snippets, s("while", fmt([[
while ({}) {{
    {}
}}]], {
  i(1, "condition"),
  i(2, "")
})))

table.insert(cpp_snippets, s("try", fmt([[
try {{
    {}
}}]], {
  i(1, "")
})))

table.insert(cpp_snippets, s("catch", fmt([[
catch ({}) {{
    {}
}}]], {
  i(1, "exception"),
  i(2, "")
})))

table.insert(cpp_snippets, s("finally", fmt([[
finally {{
    {}
}}]], {
  i(1, "")
})))

table.insert(cpp_snippets, s("namespace", fmt([[
namespace {} {{
    {}
}}]], {
  i(1, "foo::bar"),
  i(2, "")
})))

table.insert(cpp_snippets, s("namespace_anonymous", fmt([[
namespace {{
    {}
}}]], {
  i(1, "")
})))

table.insert(cpp_snippets, s("namespace_alias", {
  t("namespace "), i(1, "alias"), t(" = "), i(2, "")
}))

table.insert(cpp_snippets, s("using", {
  t("using "), i(1, "")
}))

table.insert(cpp_snippets, s("class", fmt([[
class {} {{
    {}
}};]], {
  i(1, "Name"),
  i(2, "")
})))

table.insert(cpp_snippets, s("struct", fmt([[
struct {} {{
    {}
}};]], {
  i(1, "Name"),
  i(2, "")
})))

table.insert(cpp_snippets, s("enum", fmt([[
enum {} {{
    {}
}};]], {
  i(1, "Name"),
  i(2, "")
})))

table.insert(cpp_snippets, s("enum_class", fmt([[
enum class {} {{
    {}
}};]], {
  i(1, "Name"),
  i(2, "")
})))

vim.list_extend(cpp_snippets, sm({"lambda", "lam"}, {
  t("["), i(1, ""), t("]("), i(2, ""), t("){"), i(3, ""), t("}")
}))

table.insert(cpp_snippets, s("noexcept", t("noexcept")))

-- Macros
vim.list_extend(cpp_snippets, sm({"include_library", "include"}, {
  t("#include <"), i(1, "path"), t(">")
}))

vim.list_extend(cpp_snippets, sm({"include_myown", "includeo"}, {
  t('#include "'), i(1, "path"), t('"')
}))

-- Expressions
table.insert(cpp_snippets, s("demangle", {
  t("abi::__cxa_demangle("), i(1, "#:var"), t(".name(), 0, 0, nullptr);")
}))

vim.list_extend(cpp_snippets, sm({"println", "pr"}, {
  t("std::cout << "), i(1, "something"), t(" << '\\n';")
}))

table.insert(cpp_snippets, s("print", {
  t("std::cout << "), i(1, "something"), t(";")
}))

table.insert(cpp_snippets, s("wprintln", {
  t("std::wcout << "), i(1, "something"), t(" << '\\n';")
}))

table.insert(cpp_snippets, s("wprint", {
  t("std::wcout << "), i(1, "something"), t(";")
}))

-- Templates
vim.list_extend(cpp_snippets, sm({"do_demangle", "lets_demangle", "demangle_template"}, fmt([[
#include <cxxabi.h> // copy and paste this to the top of your file
char* typeName = abi::__cxa_demangle(typeid({}).name(), 0, 0, nullptr);
{}
std::free(typeName);]], {
  i(1, "#:var"),
  i(2, "")
})))

table.insert(cpp_snippets, s("catch_std_exception", fmt([[
catch (const std::exception& e) {{
    {}
}}]], {
  i(1, "")
})))

vim.list_extend(cpp_snippets, sm({"get_type_of_exception_ptr", "exception_ptr_type"}, fmt([[
std::cout << ({} ? {}.__cxa_exception_type()->name() : "null") << std::endl;]], {
  i(1, "p"),
  i(1, "")
})))

vim.list_extend(cpp_snippets, sm({"do_at_scope_end", "finally"}, fmt([[
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
})))

table.insert(cpp_snippets, s("main", {
  t("int main() {"), i(1, "here"), t("}")
}))

vim.list_extend(cpp_snippets, sm({"unique_resource", "scoped_resource", "scoped_guard", "finally"}, fmt([[
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
}};]], {})))

vim.list_extend(cpp_snippets, sm({"vector_filter", "filter"}, fmt([[
std::vector<{}> {};
std::copy_if({}.begin(), {}.end(), std::back_inserter({}), {});]], {
  i(1, "int"),
  i(2, "zs"),
  i(3, "xs"),
  i(3, ""),
  i(2, ""),
  i(4, "predicate")
})))

table.insert(cpp_snippets, s("newtype_integral", {
  t("enum class "), i(1, "Name"), t(" : "), i(2, "int"), t(" {};")
}))

table.insert(cpp_snippets, s("newtype_integral_use", {
  i(1, "Name"), t(" "), i(2, "x"), t(" = "), i(1, ""), t("{"), i(3, "10"), t("};")
}))

return {
  snippets = cpp_snippets,
  autosnippets = {}
}