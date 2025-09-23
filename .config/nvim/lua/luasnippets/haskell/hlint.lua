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

local hlint_snippets = {}

-- General HLint pragma annotation
vim.list_extend(
  hlint_snippets,
  sm(
    { 'pragma_ann_hlint_ignore', 'hlint_ignore', 'suppress_hlint_warnings', 'ignore_hlint_warnings' },
    { t('{-# ANN '), i(1, 'target'), t(' "HLint: ignore '), i(2, 'message'), t('" #-}') },
    { key = 'pragma_ann_hlint_ignore' }
  )
)

-- Specific HLint ignore pragmas
table.insert(hlint_snippets, s('hlint_ignore_use_camel_case', t('{-# ANN module "HLint: ignore Use camelCase" #-}')))

table.insert(
  hlint_snippets,
  s('hlint_ignore_redundant_lambda', { t('{-# ANN '), i(1, 'funcName'), t(' "HLint: ignore Redundant lambda" #-}') })
)

table.insert(
  hlint_snippets,
  s('hlint_ignore_unnecessary_hiding', t('{-# ANN module "HLint: ignore Unnecessary hiding" #-}'))
)

table.insert(
  hlint_snippets,
  s('hlint_ignore_eta_reduce', { t('{-# ANN '), i(1, 'funcName'), t(' "HLint: ignore Eta reduce" #-}') })
)

table.insert(
  hlint_snippets,
  s(
    'hlint_ignore_use_newtype_instead_of_data',
    { t('{-# ANN '), i(1, 'type'), t(' "HLint: ignore Use newtype instead of data" #-}') }
  )
)

table.insert(
  hlint_snippets,
  s('hlint_ignore_evaluate', { t('{-# ANN '), i(1, 'func'), t(' "HLint: ignore Evaluate" #-}') })
)

return hlint_snippets
