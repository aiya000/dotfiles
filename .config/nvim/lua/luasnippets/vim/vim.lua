local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

local vim_snippets = {}

-- Basic syntax
vim.list_extend(vim_snippets, sm({ 'function', 'func', 'fun' }, fmt([[
  function! {}({}) abort{}
      {}
  endfunction
]], {
  i(1, 'name'),
  i(2, 'args'),
  i(3, ''),
  i(4, 'TARGET'),
})))

table.insert(vim_snippets, s('if', fmt([[
  if {}
      {}
  endif
]], {
  i(1, 'condition'),
  i(2, 'TARGET'),
})))

table.insert(vim_snippets, s('elseif', fmt([[
  elseif {}
      {}
]], {
  i(1, 'cond'),
  i(2, ''),
})))

table.insert(vim_snippets, s('else', t('else')))

table.insert(vim_snippets, s('for', fmt([[
  for {} in {}
      {}
  endfor
]], {
  i(1, 'var'),
  i(2, 'iter'),
  i(3, 'TARGET'),
})))

table.insert(vim_snippets, s('while', fmt([[
  while {}
      {}
  endwhile
]], {
  i(1, 'v:true'),
  i(2, ''),
})))

table.insert(vim_snippets, s('try', fmt([[
  try
      {}
  endtry
]], {
  i(1, ''),
})))

table.insert(vim_snippets, s('catch', fmt([[
  catch
      {}
]], {
  i(1, ''),
})))

table.insert(vim_snippets, s('finally', fmt([[
  finally
      {}
]], {
  i(1, ''),
})))

-- Expressions
vim.list_extend(vim_snippets, sm({ 'lambda', 'lam' }, fmt('{{{}}} -> {}}}', {
  i(1, 'x'),
  i(2, 'expr'),
})))

table.insert(vim_snippets, s('throw', fmt('throw \'{}\'', {
  i(1, ''),
})))

-- Operators
vim.list_extend(vim_snippets, sm({ 'equals', 'eq' }, t('==#')))
vim.list_extend(vim_snippets, sm({ 'not_equals', 'ne' }, t('!=#')))

table.insert(vim_snippets, s('augroup', fmt([[
  augroup {}
      {}
  augroup END
]], {
  i(1, 'name'),
  i(2, ''),
})))

-- Templates
vim.list_extend(vim_snippets, sm({ 'throw_not_implemented_yet', 'todo' }, fmt('throw \'Not implemented yet ({})\'', {
  i(1, 'name'),
})))

-- Assertions
vim.list_extend(vim_snippets, sm({ 'assert_equal', 'ase' }, fmt('call assert_equal({}, {})', {
  i(1, 'actual'),
  i(2, 'expected'),
})))

vim.list_extend(vim_snippets, sm({ 'assert_report', 'asr', 'assert_failure' }, fmt('call assert_report({})', {
  i(1, 'msg'),
})))

-- Debug output
vim.list_extend(vim_snippets, sm({ 'print', 'pr' }, t('caddexpr')))

vim.list_extend(vim_snippets, sm({ 'print_poi', 'poi' }, fmt('caddexpr $\'poi: {{{}}}\'', {
  i(1, 'here'),
})))

table.insert(vim_snippets, s('measure_time', fmt([[
  let start_time = reltime()
  {}
  caddexpr $'poi: Elapsed time: {{reltimestr(reltime(start_time))}} (secs)'
]], {
  i(1, ''),
})))

return { snippets = vim_snippets, autosnippets = {} }