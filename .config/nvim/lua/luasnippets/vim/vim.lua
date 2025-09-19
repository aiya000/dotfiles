local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

return list.concat(
  -- Basic syntax
  sm({ 'function', 'func', 'fun' }, fmt([[
    function! {}({}) abort{}
        {}
    endfunction
  ]], {
    i(1, 'name'),
    i(2, 'args'),
    i(3, ''),
    i(4, 'TARGET'),
  })),

  {
    s('if', fmt([[
      if {}
          {}
      endif
    ]], {
      i(1, 'condition'),
      i(2, 'TARGET'),
    })),

    s('elseif', fmt([[
      elseif {}
          {}
    ]], {
      i(1, 'cond'),
      i(2, ''),
    })),

    s('else', t('else')),

    s('for', fmt([[
      for {} in {}
          {}
      endfor
    ]], {
      i(1, 'var'),
      i(2, 'iter'),
      i(3, 'TARGET'),
    })),

    s('while', fmt([[
      while {}
          {}
      endwhile
    ]], {
      i(1, 'v:true'),
      i(2, ''),
    })),

    s('try', fmt([[
      try
          {}
      endtry
    ]], {
      i(1, ''),
    })),

    s('catch', fmt([[
      catch
          {}
    ]], {
      i(1, ''),
    })),

    s('finally', fmt([[
      finally
          {}
    ]], {
      i(1, ''),
    })),
  },

  -- Expressions
  sm({ 'lambda', 'lam' }, fmt('{{{}}} -> {}}}', {
    i(1, 'x'),
    i(2, 'expr'),
  })),

  {
    s('throw', fmt('throw \'{}\'', {
      i(1, ''),
    })),
  },

  -- Operators
  sm({ 'equals', 'eq' }, t('==#')),
  sm({ 'not_equals', 'ne' }, t('!=#')),

  {
    s('augroup', fmt([[
      augroup {}
          {}
      augroup END
    ]], {
      i(1, 'name'),
      i(2, ''),
    })),
  },

  -- Templates
  sm({ 'throw_not_implemented_yet', 'todo' }, fmt('throw \'Not implemented yet ({})\'', {
    i(1, 'name'),
  })),

  -- Assertions
  sm({ 'assert_equal', 'ase' }, fmt('call assert_equal({}, {})', {
    i(1, 'actual'),
    i(2, 'expected'),
  })),

  sm({ 'assert_report', 'asr', 'assert_failure' }, fmt('call assert_report({})', {
    i(1, 'msg'),
  })),

  -- Debug output
  sm({ 'print', 'pr' }, t('caddexpr')),

  sm({ 'print_poi', 'poi' }, fmt('caddexpr $\'poi: {{{}}}\'', {
    i(1, 'here'),
  })),

  {
    s('measure_time', fmt([[
      let start_time = reltime()
      {}
      caddexpr $'poi: Elapsed time: {{reltimestr(reltime(start_time))}} (secs)'
    ]], {
      i(1, ''),
    })),
  }
)