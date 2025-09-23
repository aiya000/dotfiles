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
  -- Method definition
  sm(
    { 'def', 'fun' },
    s(
      'def',
      fmt(
        [[
    def {}({})
      {}
    end
  ]],
        {
          i(1, 'name'),
          i(2),
          i(0),
        }
      )
    )
  ),

  -- Class definition
  s(
    'class',
    fmt(
      [[
    class {}
      {}
    end
  ]],
      {
        i(1, 'Name'),
        i(0),
      }
    )
  ),

  -- Lambda block
  sm(
    { 'lambda_block', 'lam', 'lambl' },
    s(
      'lambda_block',
      fmt(
        [[
    {{|{}| {}}}
  ]],
        {
          i(1, 'name'),
          i(0),
        }
      )
    )
  ),

  -- Lambda do block
  sm(
    { 'lambda_do', 'lamdo' },
    s(
      'lambda_do',
      fmt(
        [[
    do |{}|
      {}
    end
  ]],
        {
          i(1, 'name'),
          i(0),
        }
      )
    )
  ),

  -- Case statement
  s(
    'case',
    fmt(
      [[
    case {}
      {}
    end
  ]],
      {
        i(1, 'x'),
        i(0),
      }
    )
  ),

  -- When clause
  s('when', t('when '), i(0, 'cond')),

  -- Else clause
  s('else', t('else')),

  -- When then clause
  s(
    'when_then',
    fmt('when {} then {}', {
      i(1, 'cond'),
      i(0),
    })
  ),

  -- Raise exception
  s(
    'raise',
    fmt('raise {}', {
      i(0, 'exception'),
    })
  ),

  -- Begin rescue block
  sm(
    { 'begin', 'try' },
    s(
      'begin',
      fmt(
        [[
    begin
      {}
    rescue
      {}
    end
  ]],
        {
          i(1),
          i(0),
        }
      )
    )
  ),

  -- Rescue block
  sm(
    { 'rescue', 'catch' },
    s(
      'rescue',
      fmt(
        [[
    rescue
      {}
    end
  ]],
        {
          i(0),
        }
      )
    )
  ),

  -- Do block
  s(
    'do',
    fmt(
      [[
    do
      {}
    end
  ]],
      {
        i(0, 'here'),
      }
    )
  ),

  -- Do block with parameters
  sm(
    { 'do_with_params', 'dp' },
    s(
      'do_with_params',
      fmt(
        [[
    do |{}|
      {}
    end
  ]],
        {
          i(1, 'params'),
          i(0, 'here'),
        }
      )
    )
  ),

  -- Quoted string
  s(
    'q',
    fmt('%Q({})', {
      i(0, 'here'),
    })
  ),

  -- Heredoc
  s(
    'heredoc',
    fmt(
      [[
    <<-{}
    {}
    {}
  ]],
      {
        i(1, 'EOS'),
        i(0),
        rep(1),
      }
    )
  ),

  -- Heredoc with strip margins
  sm(
    { 'heredoc_strip_margins', 'here' },
    s(
      'heredoc_strip_margins',
      fmt(
        [[
    <<~{}
    {}
    {}
  ]],
        {
          i(1, 'EOS'),
          i(0),
          rep(1),
        }
      )
    )
  ),

  -- For loop
  s(
    'for',
    fmt(
      [[
    for {} in {} do
      {}
    end
  ]],
      {
        i(1, 'x'),
        i(2, 'xs'),
        i(0),
      }
    )
  ),

  -- If statement
  s(
    'if',
    fmt(
      [[
    if {} then
      {}
    end
  ]],
      {
        i(1, 'cond'),
        i(0),
      }
    )
  ),

  -- Require statement
  sm(
    { 'require', 'import', 'imp' },
    s(
      'require',
      fmt("require '{}'", {
        i(0, 'here'),
      })
    )
  ),

  -- Puts expression
  s(
    'pr',
    fmt('puts {}', {
      i(0),
    })
  ),
}
