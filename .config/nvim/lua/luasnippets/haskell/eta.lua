local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require('luasnip.extras.fmt').fmt

local sm = function(trigger_table, nodes, opts)
  local result = {}
  for _, trigger in ipairs(trigger_table) do
    table.insert(result, s(trigger, nodes, opts))
  end
  return result
end

local eta_snippets = {}

-- Pragma snippets
vim.list_extend(
  eta_snippets,
  sm(
    { 'pragma_class', 'eta_class_pragma' },
    { t('{-# CLASS "'), i(1, 'java.lang.Integer'), t('" #-}') },
    { key = 'pragma_class' }
  )
)

-- Data type snippets
vim.list_extend(
  eta_snippets,
  sm({ 'data_of_java_old', 'eta_data_old', 'data_eta_old' }, {
    t('data {-# CLASS "'),
    i(1, 'java.lang.String'),
    t('" #-} '),
    i(2, 'JString'),
    t(' ='),
    t({ '', '    ' }),
    i(2),
    t(' (Object# '),
    i(2),
    t(')'),
    t({ '', '    deriving (Class)' }),
  }, { key = 'data_of_java_old' })
)

vim.list_extend(
  eta_snippets,
  sm({ 'data_of_java', 'eta_data', 'data_eta' }, {
    t('data '),
    i(1, 'JString'),
    t(' = '),
    i(1),
    t(' @'),
    i(2, 'java.lang.String'),
    t({ '', '    deriving (Class)' }),
  }, { key = 'data_of_java' })
)

vim.list_extend(
  eta_snippets,
  sm({ 'data_of_java_array', 'eta_array', 'array_eta' }, {
    t('data {-# CLASS "'),
    i(1, 'java.lang.String'),
    t('[]" #-} J'),
    i(2, 'String'),
    t('Array ='),
    t({ '', '    J' }),
    i(2),
    t('Array (Object# J'),
    i(2),
    t('Array)'),
    t({ '', '    deriving (Class)', '', '' }),
    t('instance JArray '),
    i(2),
    t(' J'),
    i(2),
    t('Array'),
  }, { key = 'data_of_java_array' })
)

-- Foreign import snippets
vim.list_extend(
  eta_snippets,
  sm({ 'foreign_import_a_java', 'import_foreign_a_java' }, {
    t('foreign import java unsafe '),
    i(1, '#:"some annotation" '),
    i(2, 'fieldName'),
    t(' ::'),
    t({ '', '    Java ' }),
    i(3, 'AssocType'),
    t(' '),
    i(4, 'ReturnType'),
  }, { key = 'foreign_import_a_java' })
)

vim.list_extend(
  eta_snippets,
  sm({ 'foreign_import_a_java_constructor', 'import_foreign_a_java_constructor', 'eta_new_a_class', 'new_function' }, {
    t('foreign import java unsafe "@new" new'),
    i(1, 'Name'),
    t(' ::'),
    t({ '', '    ' }),
    i(2, '#:arguments ->'),
    t('Java a '),
    i(3, 'ClassName'),
    i(0),
  }, { key = 'foreign_import_a_java_constructor' })
)

vim.list_extend(
  eta_snippets,
  sm({ 'foreign_import_a_java_method', 'import_foreign_a_java_method', 'eta_method_function_side_java' }, {
    t('foreign import java unsafe "'),
    i(1, 'originalMethodName'),
    t('" '),
    i(2, 'methodName'),
    t(' ::'),
    t({ '', '    ' }),
    i(3, '#:arguments ->'),
    t('Java '),
    i(4, 'AssocType'),
    t(' '),
    i(5, 'ReturnType'),
  }, { key = 'foreign_import_a_java_method' })
)

vim.list_extend(
  eta_snippets,
  sm({ 'foreign_import_a_java_field', 'import_foreign_a_java_field', 'eta_field' }, {
    t('foreign import java unsafe "@field '),
    i(1, 'fieldName'),
    t('" '),
    i(2, 'fieldNameInHs'),
    t(' ::'),
    t({ '', '    ' }),
    i(3, 'Java ClassName Int'),
  }, { key = 'foreign_import_a_java_field' })
)

vim.list_extend(
  eta_snippets,
  sm(
    {
      'foreign_import_a_java_static_field',
      'import_foreign_a_java_static_field',
      'eta_static_field',
      'import_foreign_a_java_enum_term',
      'eta_enum_term',
      'enum_term_eta',
    },
    {
      t('foreign import java unsafe "@static @field '),
      i(1, 'fieldName'),
      t('" '),
      i(2, 'fieldNameInHs'),
      t(' ::'),
      t({ '', '    ' }),
      i(3, 'Java ClassName Int'),
    },
    { key = 'foreign_import_a_java_static_field' }
  )
)

vim.list_extend(
  eta_snippets,
  sm({ 'foreign_export_a_java_method', 'eta_method_function_side_haskell' }, {
    t('foreign export java "'),
    i(1, 'methodName'),
    t('" '),
    i(1),
    t(' ::'),
    t({ '', '    ' }),
    i(2, '#:arguments ->'),
    t('Java '),
    i(3, 'ReceiverJClass'),
    t(' '),
    i(5, 'ReturnType'),
  }, { key = 'foreign_export_a_java_method' })
)

return eta_snippets
