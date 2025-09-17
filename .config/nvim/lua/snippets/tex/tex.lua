local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

return {
  -- Document structure
  s('begin-end', fmt([[
    \begin{{{}}}
        {}
    \end{{{}}}
  ]], {
    i(1, ''),
    i(2, ''),
    i(3, ''),
  })),

  sm({ 'template-base', 'base' }, fmt([[
    \documentclass{{jsarticle}}
    \begin{{document}}
        {}
    \end{{document}}
  ]], {
    i(1, ''),
  })),

  s('title', fmt([[
    \title{{{}}}
    \author{{{}}}
    \maketitle
  ]], {
    i(1, 'title'),
    i(2, 'name'),
  })),

  -- Package management
  s('use', fmt('\\usepackage{{{}}}', {
    i(1, 'name'),
  })),

  s('usepackage', fmt('\\usepackage[{{{}}}]{{{}}}', {
    i(1, 'options'),
    i(2, 'name'),
  })),

  -- Sectioning
  s('section', fmt('\\section{{{}}}{}', {
    i(1, 'subject'),
    i(2, ''),
  })),

  -- Text formatting
  sm({ 'texttt', 'ttt' }, fmt('\\texttt{{{}}}', {
    i(1, ''),
  })),

  -- Math environments
  s('equation', fmt([[
    \begin{{equation}}
        {}
    \end{{equation}}
  ]], {
    i(1, ''),
  })),

  s('eqnarray', fmt([[
    \begin{{eqnarray}}
        {}
    \end{{eqnarray}}
  ]], {
    i(1, ''),
  })),

  s('multicols', fmt([[
    \begin{{multicols}}{{{}}}
        {}
    \end{{multicols}}
  ]], {
    i(1, 'cols_num'),
    i(2, ''),
  })),

  s('expr', fmt('\\[{}\\]', {
    i(1, ''),
  })),

  -- Math symbols and structures
  s('position', fmt('&{}&{}', {
    i(1, ''),
    i(2, ''),
  })),

  s('set', fmt('\\{{{}\\}}', {
    i(1, ''),
  })),

  s('bracket', fmt('\\left({}\\right)', {
    i(1, ''),
  })),
}