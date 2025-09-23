local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

local tex_snippets = {}

-- Document structure
table.insert(
  tex_snippets,
  s(
    'begin-end',
    fmt(
      [[
  \begin{{{}}}
      {}
  \end{{{}}}
]],
      {
        i(1, ''),
        i(2, ''),
        i(3, ''),
      }
    )
  )
)

vim.list_extend(
  tex_snippets,
  sm(
    { 'template-base', 'base' },
    fmt(
      [[
  \documentclass{{jsarticle}}
  \begin{{document}}
      {}
  \end{{document}}
]],
      {
        i(1, ''),
      }
    )
  )
)

table.insert(
  tex_snippets,
  s(
    'title',
    fmt(
      [[
  \title{{{}}}
  \author{{{}}}
  \maketitle
]],
      {
        i(1, 'title'),
        i(2, 'name'),
      }
    )
  )
)

-- Package management
table.insert(
  tex_snippets,
  s(
    'use',
    fmt('\\usepackage{{{}}}', {
      i(1, 'name'),
    })
  )
)

table.insert(
  tex_snippets,
  s(
    'usepackage',
    fmt('\\usepackage[{{{}}}]{{{}}}', {
      i(1, 'options'),
      i(2, 'name'),
    })
  )
)

-- Sectioning
table.insert(
  tex_snippets,
  s(
    'section',
    fmt('\\section{{{}}}{}', {
      i(1, 'subject'),
      i(2, ''),
    })
  )
)

-- Text formatting
vim.list_extend(
  tex_snippets,
  sm(
    { 'texttt', 'ttt' },
    fmt('\\texttt{{{}}}', {
      i(1, ''),
    })
  )
)

-- Math environments
table.insert(
  tex_snippets,
  s(
    'equation',
    fmt(
      [[
  \begin{{equation}}
      {}
  \end{{equation}}
]],
      {
        i(1, ''),
      }
    )
  )
)

table.insert(
  tex_snippets,
  s(
    'eqnarray',
    fmt(
      [[
  \begin{{eqnarray}}
      {}
  \end{{eqnarray}}
]],
      {
        i(1, ''),
      }
    )
  )
)

table.insert(
  tex_snippets,
  s(
    'multicols',
    fmt(
      [[
  \begin{{multicols}}{{{}}}
      {}
  \end{{multicols}}
]],
      {
        i(1, 'cols_num'),
        i(2, ''),
      }
    )
  )
)

table.insert(
  tex_snippets,
  s(
    'expr',
    fmt('\\[{}\\]', {
      i(1, ''),
    })
  )
)

-- Math symbols and structures
table.insert(
  tex_snippets,
  s(
    'position',
    fmt('&{}&{}', {
      i(1, ''),
      i(2, ''),
    })
  )
)

table.insert(
  tex_snippets,
  s(
    'set',
    fmt('\\{{{}\\}}', {
      i(1, ''),
    })
  )
)

table.insert(
  tex_snippets,
  s(
    'bracket',
    fmt('\\left({}\\right)', {
      i(1, ''),
    })
  )
)

return { snippets = tex_snippets, autosnippets = {} }
