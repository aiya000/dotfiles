local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

local snippets = {}

-- Add all snippets with proper expansion
vim.list_extend(
  snippets,
  sm(
    { 'package', 'pack' },
    fmt('package {}', {
      i(1, 'packageName'),
    })
  )
)

vim.list_extend(
  snippets,
  sm(
    { 'func', 'fun' },
    fmt(
      [[
  func {name}({args}){return_type} {{
      {}
  }}
]],
      {
        name = i(1, 'funcName'),
        args = i(2, ''),
        return_type = i(3, ''),
        i(4, 'return'),
      }
    )
  )
)

vim.list_extend(
  snippets,
  sm(
    { 'import', 'imp' },
    fmt(
      [[
  import (
      "{}"
  )
  {}
]],
      {
        i(1, 'fmt'),
        i(2, ''),
      }
    )
  )
)

table.insert(
  snippets,
  s(
    'if',
    fmt('if {} {{{}}}', {
      i(1, 'cond'),
      i(2, 'here'),
    })
  )
)

table.insert(
  snippets,
  s(
    'else',
    fmt('else {{{}}}', {
      i(1, 'here'),
    })
  )
)

table.insert(
  snippets,
  s(
    'array_type',
    fmt('[]{}', {
      i(1, 'TypeName'),
    })
  )
)

vim.list_extend(
  snippets,
  sm(
    { 'array_constant', 'array' },
    fmt('[]{} {{{}}}', {
      i(1, 'TypeName'),
      i(2, 'here'),
    })
  )
)

vim.list_extend(
  snippets,
  sm(
    { 'val', 'const' },
    fmt('{} := {}', {
      i(1, 'varName'),
      i(2, ''),
    })
  )
)

vim.list_extend(
  snippets,
  sm(
    { 'init_global_var', 'var' },
    fmt('var {} = {}', {
      i(1, 'varName'),
      i(2, ''),
    })
  )
)

vim.list_extend(
  snippets,
  sm(
    { 'var_array', 'var_slice' },
    fmt('var {} [{}]{}', {
      i(1, 'varName'),
      i(2, 'size'),
      i(3, 'TypeName'),
    })
  )
)

table.insert(
  snippets,
  s(
    'type',
    fmt('type {}', {
      i(1, 'Name'),
    })
  )
)

vim.list_extend(
  snippets,
  sm(
    { 'struct', 'str', 'stru' },
    fmt('struct {{{}}}', {
      i(1, 'here'),
    })
  )
)

table.insert(
  snippets,
  s(
    'type_assert',
    fmt('{}, {} := {}.({})', {
      i(1, 'extractedVarName'),
      i(2, 'isItExpectedType'),
      i(3, 'varName'),
      i(4, 'expectedType'),
    })
  )
)

table.insert(
  snippets,
  s(
    'defer',
    fmt('defer {}', {
      i(1, 'expr'),
    })
  )
)

table.insert(
  snippets,
  s(
    'for',
    fmt('for {}, {} := range {} {{{}}}', {
      i(1, '_'),
      i(2, 'name'),
      i(3, 'array'),
      i(4, 'here'),
    })
  )
)

table.insert(
  snippets,
  s(
    'switch',
    fmt('switch {} {{{}}}', {
      i(1, 'x'),
      i(2, 'here'),
    })
  )
)

table.insert(
  snippets,
  s(
    'case',
    fmt('case {}:{}', {
      i(1, 'pattern'),
      i(2, ''),
    })
  )
)

-- Expressions
table.insert(
  snippets,
  s(
    'fprint',
    fmt('fmt.Fprint({}, {})', {
      i(1, 'os.Stderr'),
      i(2, 'err'),
    })
  )
)

table.insert(
  snippets,
  s(
    'fprintln',
    fmt('fmt.Fprintln({}, {})', {
      i(1, 'os.Stderr'),
      i(2, 'err'),
    })
  )
)

vim.list_extend(
  snippets,
  sm(
    { 'println', 'pr' },
    fmt('fmt.Println({})', {
      i(1, 'here'),
    })
  )
)

vim.list_extend(
  snippets,
  sm(
    { 'log_println', 'log' },
    fmt('log.Println({})', {
      i(1, 'here'),
    })
  )
)

table.insert(
  snippets,
  s(
    'make',
    fmt('make([]{}, {})', {
      i(1, 'type'),
      i(2, 'len'),
    })
  )
)

table.insert(
  snippets,
  s(
    'map_type',
    fmt('map[{}]{}', {
      i(1, 'KeyType'),
      i(2, 'ValueType'),
    })
  )
)

table.insert(
  snippets,
  s(
    'make_map',
    fmt('make(map[{}]{})', {
      i(1, 'KeyType'),
      i(2, 'ValueType'),
    })
  )
)

-- Others
vim.list_extend(
  snippets,
  sm(
    { 'document_comment', 'doc' },
    fmt(
      [[
  /**
   * {}
   */
]],
      {
        i(1, ''),
      }
    )
  )
)

vim.list_extend(
  snippets,
  sm(
    { 'if_err', 'ife' },
    fmt('if err != nil {{{}}}', {
      i(1, 'here'),
    })
  )
)

table.insert(
  snippets,
  s(
    'append',
    fmt('{xs} = append({xs}, {})', {
      xs = i(1, 'xs'),
      i(2, ''),
    })
  )
)

return {
  snippets = snippets,
  autosnippets = {},
}
