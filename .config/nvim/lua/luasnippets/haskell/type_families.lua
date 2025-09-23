local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

local sm = function(trigger_table, nodes, opts)
  local result = {}
  for _, trigger in ipairs(trigger_table) do
    table.insert(result, s(trigger, nodes, opts))
  end
  return result
end

local type_families_snippets = {}

-- Basic type family declaration
vim.list_extend(
  type_families_snippets,
  sm({ 'type_family', 'type_func_declare' }, t('type family'), { key = 'type_family' })
)

-- Type instance
vim.list_extend(
  type_families_snippets,
  sm(
    { 'type_instance', 'type_func_define' },
    { t('type instance '), i(1, '#:Pattern'), t(' '), i(2, 'a'), t(' = '), i(0) },
    { key = 'type_instance' }
  )
)

-- Type family with kind signature
vim.list_extend(
  type_families_snippets,
  sm(
    { 'type_family_kind', 'type_func_declare_kind' },
    { t('type family '), i(1, '#:TypeName'), t(' '), i(2, 'a'), t(' :: '), i(0, '*') },
    { key = 'type_family_kind' }
  )
)

-- Type family with GADTs syntax
vim.list_extend(
  type_families_snippets,
  sm({ 'type_family_gadts', 'type_func_declare_gadts' }, {
    t('type family '),
    i(1, '#:TypeName'),
    t(' '),
    i(2, 'a'),
    t(' where'),
    t({ '', '    ' }),
    i(1),
    t(' = '),
    i(0),
  }, { key = 'type_family_gadts' })
)

-- Type family with GADTs and kind signature
vim.list_extend(
  type_families_snippets,
  sm({ 'type_family_gadts_kind', 'type_func_define_gadts_kind' }, {
    t('type family '),
    i(1, '#:TypeName'),
    t(' '),
    i(2, 'a'),
    t(' :: '),
    i(3, '*'),
    t(' where'),
    t({ '', '    ' }),
    i(1),
    t(' '),
    i(4, '#:args'),
    t(' = '),
    i(0),
  }, { key = 'type_family_gadts_kind' })
)

return type_families_snippets
