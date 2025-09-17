local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

return {
  -- JSDoc-style annotations for Vim scripts
  s('typedef', fmt('@typedef {{{}}} {}', {
    i(2, 'type'),
    i(1, 'name'),
  })),

  sm({ 'property', 'prop' }, fmt('@property {{{}}} {}', {
    i(2, 'type'),
    i(1, 'name'),
  })),

  sm({ 'property_optional', 'opt_prop' }, fmt('@property {{{}}} [{}]', {
    i(2, 'type'),
    i(1, 'name'),
  })),

  s('type', fmt('@type {{{}}}', {
    i(1, 'type'),
  })),

  sm({ 'template', 'generic', 'generics' }, fmt('@template {}', {
    i(1, 'T'),
  })),

  sm({ 'template_default', 'generic_default', 'generics_default' }, fmt('@template [{}={}]', {
    i(1, 'T'),
    i(2, 'Default'),
  })),

  s('param', fmt('@param {} {{{}}}', {
    i(1, 'name'),
    i(2, 'type'),
  })),

  sm({ 'param_optional', 'opt', 'opt_param' }, fmt('@param [{}] {{{}}}', {
    i(1, 'name'),
    i(2, 'type'),
  })),

  sm({ 'returns', 'return' }, fmt('@returns {{{}}}', {
    i(1, 'type'),
  })),

  s('throws', t('@throws')),
}