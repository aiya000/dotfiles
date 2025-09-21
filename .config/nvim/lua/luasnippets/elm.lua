local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

local snippets = {}

-- Syntax
vim.list_extend(snippets, sm({ 'import', 'imp' }, fmt('import {}', {
  i(1, 'ModuleName'),
})))

vim.list_extend(snippets, sm({ 'exposing', 'exp' }, fmt('exposing ({})', {
  i(1, 'stuffs'),
})))

vim.list_extend(snippets, sm({ 'import_as', 'import_qualified', 'imq' }, fmt('import {} as {}', {
  i(1, 'Name'),
  i(2, 'Name'),
})))

table.insert(snippets, s('type', fmt('type {} = {}', {
  i(1, 'Name'),
  i(2, ''),
})))

vim.list_extend(snippets, sm({ 'update_record', 'update', 'up' }, fmt('{{ {} | {} = {} }}', {
  i(1, 'object'),
  i(2, 'record'),
  i(3, 'value'),
})))

table.insert(snippets, s('let', fmt([[
  let {} = {}
  in {}
]], {
  i(1, 'var'),
  i(2, 'value'),
  i(3, ''),
})))

table.insert(snippets, s('case', fmt([[
  case {} of
      {} -> {}
]], {
  i(1, 'var'),
  i(2, 'pattern'),
  i(3, ''),
})))

table.insert(snippets, s('if', fmt('if {} then {} else {}', {
  i(1, 'cond'),
  i(2, 'expr'),
  i(3, 'expr'),
})))

table.insert(snippets, s('module', fmt('module {} exposing ({})', {
  i(1, 'Name'),
  i(2, 'here'),
})))

vim.list_extend(snippets, sm({ 'lambda', 'lam' }, fmt('\\{} -> {}', {
  i(1, 'var'),
  i(2, ''),
})))

-- Expression
table.insert(snippets, s('to', t('|>')))

table.insert(snippets, s('from', t('<|')))

table.insert(snippets, s('div', fmt('div [{}] [{}]', {
  i(1, 'stuff'),
  i(2, 'stuff'),
})))

table.insert(snippets, s('undefined', t('Debug.crash "undefined"')))

-- Other
vim.list_extend(snippets, sm({ 'document_comment', 'doc' }, fmt('{{-|{}-}}', {
  i(1, 'here'),
})))

return {
  snippets = snippets,
  autosnippets = {}
}