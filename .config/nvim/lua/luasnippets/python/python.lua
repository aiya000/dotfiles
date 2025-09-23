local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require('luasnip.extras.fmt').fmt

local function sm(trigger, nodes, aliases)
  local snippets = { s(trigger, nodes) }
  for _, alias in ipairs(aliases or {}) do
    table.insert(snippets, s(alias, nodes))
  end
  return snippets
end

-- Convert to proper array structure for LuaSnip compatibility
local python_snippets = {}

-- Syntaxes
table.insert(python_snippets, s('import_single', fmt('import {}', { i(1, 'module') })))

vim.list_extend(
  python_snippets,
  sm('import_as', fmt('import {} as {}', { i(1, 'module'), i(2, 'alias') }), { 'import_qualified', 'imq' })
)

vim.list_extend(
  python_snippets,
  sm('from', fmt('from {} import {}', { i(1, 'module'), i(2, 'stuff') }), { 'import', 'imp' })
)

table.insert(python_snippets, s('if', fmt('if {}:{}', { i(1), i(0) })))

table.insert(python_snippets, s('else', t('else:')))

table.insert(python_snippets, s('for', fmt('for {} in {}:{}', { i(1, 'x'), i(2, 'xs'), i(0) })))

table.insert(python_snippets, s('while', fmt('while {}:{}', { i(1, 'cond'), i(0) })))

vim.list_extend(
  python_snippets,
  sm('def', fmt('def {}({}) -> {}:{}', { i(1, 'name'), i(2, '#:self'), i(3, 'type'), i(0) }), { 'fun' })
)

vim.list_extend(
  python_snippets,
  sm('class', fmt('class {}{}:{}', { i(1, 'Name'), i(2, '#:(Super)'), i(0) }), { 'cla' })
)

vim.list_extend(
  python_snippets,
  sm(
    'conditional_operator',
    fmt('{} if {} else {}', { i(1, 'value_if_true'), i(2, 'cond'), i(3, 'value_if_false') }),
    { 'cond' }
  )
)

vim.list_extend(python_snippets, sm('lambda', fmt('lambda {}: {}', { i(1, 'args'), i(0) }), { 'lam' }))

table.insert(python_snippets, s('raise', fmt('raise {}({})', { i(1, 'Exception'), i(2, 'msg') })))

table.insert(python_snippets, s('try', t('try:')))

vim.list_extend(
  python_snippets,
  sm('except', fmt('except{}: {}', { i(1, '#: ErrorType'), i(0) }), { 'catch', 'handle' })
)

vim.list_extend(
  python_snippets,
  sm('list_comprehension', fmt('[{} for {} in {}]', { i(3, 'result'), i(1, 'var'), i(2, 'source') }), { 'list' })
)

-- Templates
vim.list_extend(python_snippets, sm('print', fmt('print({})', { i(0) }), { 'pr' }))

vim.list_extend(
  python_snippets,
  sm(
    '__init__',
    fmt(
      [[def __init__(self{}) -> None:
    {}]],
      { i(1, '#:, x'), i(0) }
    ),
    { 'init' }
  )
)

vim.list_extend(
  python_snippets,
  sm(
    '__post_init__',
    fmt(
      [[def __post_init__(self{}) -> None:
    {}]],
      { i(1, '#:, x'), i(0) }
    ),
    { 'post_init' }
  )
)

vim.list_extend(
  python_snippets,
  sm(
    '__str__',
    fmt(
      [[def __str__(self) -> str:
    {}]],
      { i(0) }
    ),
    { '__str' }
  )
)

table.insert(
  python_snippets,
  s(
    'if_name_is_main',
    fmt(
      [[if __name__ == '__main__':
    {}]],
      { i(0) }
    )
  )
)

return { snippets = python_snippets, autosnippets = {} }
