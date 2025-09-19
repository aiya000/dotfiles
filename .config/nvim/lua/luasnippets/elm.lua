local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

return {
  -- Syntax
  sm({ 'import', 'imp' }, fmt('import {}', {
    i(1, 'ModuleName'),
  })),

  sm({ 'exposing', 'exp' }, fmt('exposing ({})', {
    i(1, 'stuffs'),
  })),

  sm({ 'import_as', 'import_qualified', 'imq' }, fmt('import {} as {}', {
    i(1, 'Name'),
    i(2, 'Name'),
  })),

  s('type', fmt('type {} = {}', {
    i(1, 'Name'),
    i(2, ''),
  })),

  sm({ 'update_record', 'update', 'up' }, fmt('{{ {} | {} = {} }}', {
    i(1, 'object'),
    i(2, 'record'),
    i(3, 'value'),
  })),

  s('let', fmt([[
    let {} = {}
    in {}
  ]], {
    i(1, 'var'),
    i(2, 'value'),
    i(3, ''),
  })),

  s('case', fmt([[
    case {} of
        {} -> {}
  ]], {
    i(1, 'var'),
    i(2, 'pattern'),
    i(3, ''),
  })),

  s('if', fmt('if {} then {} else {}', {
    i(1, 'cond'),
    i(2, 'expr'),
    i(3, 'expr'),
  })),

  s('module', fmt('module {} exposing ({})', {
    i(1, 'Name'),
    i(2, 'here'),
  })),

  sm({ 'lambda', 'lam' }, fmt('\\{} -> {}', {
    i(1, 'var'),
    i(2, ''),
  })),

  -- Expression
  s('to', t('|>')),

  s('from', t('<|')),

  s('div', fmt('div [{}] [{}]', {
    i(1, 'stuff'),
    i(2, 'stuff'),
  })),

  s('undefined', t('Debug.crash "undefined"')),

  -- Other
  sm({ 'document_comment', 'doc' }, fmt('{{-|{}-}}', {
    i(1, 'here'),
  })),
}