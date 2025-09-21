local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

local vim_check_snippets = {}

-- JSDoc-style annotations for Vim scripts
table.insert(vim_check_snippets, s('typedef', fmt('@typedef {{{}}} {}', {
  i(2, 'type'),
  i(1, 'name'),
})))

vim.list_extend(vim_check_snippets, sm({ 'property', 'prop' }, fmt('@property {{{}}} {}', {
  i(2, 'type'),
  i(1, 'name'),
})))

vim.list_extend(vim_check_snippets, sm({ 'property_optional', 'opt_prop' }, fmt('@property {{{}}} [{}]', {
  i(2, 'type'),
  i(1, 'name'),
})))

table.insert(vim_check_snippets, s('type', fmt('@type {{{}}}', {
  i(1, 'type'),
})))

vim.list_extend(vim_check_snippets, sm({ 'template', 'generic', 'generics' }, fmt('@template {}', {
  i(1, 'T'),
})))

vim.list_extend(vim_check_snippets, sm({ 'template_default', 'generic_default', 'generics_default' }, fmt('@template [{}={}]', {
  i(1, 'T'),
  i(2, 'Default'),
})))

table.insert(vim_check_snippets, s('param', fmt('@param {} {{{}}}', {
  i(1, 'name'),
  i(2, 'type'),
})))

vim.list_extend(vim_check_snippets, sm({ 'param_optional', 'opt', 'opt_param' }, fmt('@param [{}] {{{}}}', {
  i(1, 'name'),
  i(2, 'type'),
})))

vim.list_extend(vim_check_snippets, sm({ 'returns', 'return' }, fmt('@returns {{{}}}', {
  i(1, 'type'),
})))

table.insert(vim_check_snippets, s('throws', t('@throws')))

return { snippets = vim_check_snippets, autosnippets = {} }