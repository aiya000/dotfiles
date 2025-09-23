-- C-like syntax snippets for neosnippet
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

return {
  s(
    { trig = 'snippet_block_syntax', namr = 'snippet_block_syntax', dscr = 'Block syntax snippet template' },
    fmt(
      [[
snippet {name}
abbr {name} ${{0:#:here}}
  {name} ${{0:#:here}}]],
      {
        name = i(1, 'syntaxName'),
      }
    )
  ),

  s(
    {
      trig = 'snippet_parens_and_block_syntax',
      namr = 'snippet_parens_and_block_syntax',
      dscr = 'Parentheses and block syntax snippet template',
    },
    fmt(
      [[
snippet {name}
abbr {name} (${{1:{kind}}}) ${{0:#:here}}
  {name} (${{1:{kind}}}) ${{0:#:here}}]],
      {
        name = i(1, 'syntaxName'),
        kind = i(2, 'kind'),
      }
    )
  ),
}
