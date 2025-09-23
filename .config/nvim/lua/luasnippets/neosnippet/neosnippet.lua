-- neosnippet snippets for neosnippet file format
local ls = require('luasnip')
local s = ls.snippet
local sn = ls.snippet_node
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local c = ls.choice_node
local d = ls.dynamic_node
local r = ls.restore_node
local l = require('luasnip.extras').lambda
local rep = require('luasnip.extras').rep
local p = require('luasnip.extras').partial
local m = require('luasnip.extras').match
local n = require('luasnip.extras').nonempty
local dl = require('luasnip.extras').dynamic_lambda
local fmt = require('luasnip.extras.fmt').fmt
local fmta = require('luasnip.extras.fmt').fmta
local types = require('luasnip.util.types')
local conds = require('luasnip.extras.expand_conditions')

local function sm(trigger, names)
  local snippets = {}
  for _, name in ipairs(names) do
    table.insert(snippets, s(name, t(trigger)))
  end
  return snippets
end

return {
  s(
    { trig = 'snippet', namr = 'snippet', dscr = 'Basic snippet template' },
    fmt(
      [[
snippet {name}
abbr {abbr}
  {content}{final}]],
      {
        name = i(1, 'name'),
        abbr = i(2),
        content = rep(2),
        final = i(0, ''),
      }
    )
  ),

  s(
    { trig = 'snip', namr = 'snip', dscr = 'Basic snippet template (alias)' },
    fmt(
      [[
snippet {name}
abbr {abbr}
  {content}{final}]],
      {
        name = i(1, 'name'),
        abbr = i(2),
        content = rep(2),
        final = i(0, ''),
      }
    )
  ),

  s(
    { trig = 'snippet_indent', namr = 'snippet_indent', dscr = 'Indented snippet template' },
    fmt(
      [[
snippet {name}
options indent
  {final}]],
      {
        name = i(1, 'name'),
        final = i(0, ''),
      }
    )
  ),

  s(
    { trig = 'snip_indent', namr = 'snip_indent', dscr = 'Indented snippet template (alias)' },
    fmt(
      [[
snippet {name}
options indent
  {final}]],
      {
        name = i(1, 'name'),
        final = i(0, ''),
      }
    )
  ),

  s(
    { trig = 'comment_example', namr = 'comment_example', dscr = 'Comment example template' },
    fmt(
      [[
# Example:
# ```{lang}
# {content}
# ```]],
      {
        lang = i(1, 'lang'),
        content = i(0, ''),
      }
    )
  ),

  s(
    { trig = 'example', namr = 'example', dscr = 'Comment example template (alias)' },
    fmt(
      [[
# Example:
# ```{lang}
# {content}
# ```]],
      {
        lang = i(1, 'lang'),
        content = i(0, ''),
      }
    )
  ),
}
