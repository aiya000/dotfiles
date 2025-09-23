local ls = require('luasnip')
local s = ls.snippet
local sn = ls.snippet_node
local isn = ls.indent_snippet_node
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local c = ls.choice_node
local d = ls.dynamic_node
local r = ls.restore_node
local events = require('luasnip.util.events')
local ai = require('luasnip.nodes.absolute_indexer')
local extras = require('luasnip.extras')
local l = extras.lambda
local rep = extras.rep
local p = extras.partial
local m = extras.match
local n = extras.nonempty
local dl = extras.dynamic_lambda
local fmt = require('luasnip.extras.fmt').fmt
local fmta = require('luasnip.extras.fmt').fmta
local conds = require('luasnip.extras.expand_conditions')
local postfix = require('luasnip.extras.postfix').postfix
local types = require('luasnip.util.types')
local parse = require('luasnip.util.parser').parse_snippet
local ms = ls.multi_snippet
local k = require('luasnip.nodes.key_indexer').new_key

local sm = function(triggers, snippet_def)
  return ms(triggers, { snippet_def })
end

return {
  -- Scalastyle suppress check for a single line
  s(
    'scalastyle_suppress_check_a_line',
    fmt('// scalastyle:ignore {}', {
      i(1, 'null'),
    })
  ),

  -- Scalastyle suppress check for a range
  s(
    'scalastyle_suppress_check_a_range',
    fmt(
      [[
    // scalastyle:off {}
    {}
    // scalastyle:on {}
  ]],
      {
        i(1, 'null'),
        i(0),
        rep(1),
      }
    )
  ),

  -- Scalastyle suppress indentation
  s('scalastyle_suppress_indentation', t('// scalastyle:ignore indentation')),

  -- Scalastyle suppress regex
  s('scalastyle_suppress_regex', t('// scalastyle:ignore regex')),
}
