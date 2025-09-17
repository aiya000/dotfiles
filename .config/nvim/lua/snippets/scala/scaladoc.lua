local ls = require('luasnip')
local s = ls.snippet
local sn = ls.snippet_node
local isn = ls.indent_snippet_node
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local c = ls.choice_node
local d = ls.dynamic_node
local r = ls.restore_node
local events = require('luasnip.util.events')
local ai = require('luasnip.nodes.absolute_indexer')
local extras = require('luasnip.extras')
local l = extras.lambda
local rep = extras.rep
local p = extras.partial
local m = extras.match
local n = extras.nonempty
local dl = extras.dynamic_lambda
local fmt = require('luasnip.extras.fmt').fmt
local fmta = require('luasnip.extras.fmt').fmta
local conds = require('luasnip.extras.expand_conditions')
local postfix = require('luasnip.extras.postfix').postfix
local types = require('luasnip.util.types')
local parse = require('luasnip.util.parser').parse_snippet
local ms = ls.multi_snippet
local k = require('luasnip.nodes.key_indexer').new_key

local sm = function(triggers, snippet_def)
  return ms(triggers, { snippet_def })
end

return {
  -- Scaladoc comment
  sm({ 'scaladoc_comment', 'doc' }, s('scaladoc_comment', fmt([[
    /**
     * {}
     */
  ]], {
    i(0)
  }))),

  -- Scaladoc comment format
  sm({ 'scaladoc_comment_format', 'doc_format' }, s('scaladoc_comment_format', fmt([[
     === Exceptions ===
     * {}
     *
     * === Failures ===
     * {}
     *
     * === Example ===
     * {{{{
     * {}
     * }}}}
  ]], {
    i(2),
    i(3),
    i(4)
  }))),

  -- Scaladoc exceptions section
  sm({ 'scaladoc_comment_format_exceptions', 'doc_format_exceptions' }, s('scaladoc_comment_format_exceptions', fmt([[
     === Exceptions ===
     * {}
  ]], {
    i(0)
  }))),

  -- Scaladoc failures section
  sm({ 'scaladoc_comment_format_failures', 'doc_format_failures' }, s('scaladoc_comment_format_failures', fmt([[
     === Failures ===
     * {}
  ]], {
    i(0)
  }))),

  -- Scaladoc example section
  sm({ 'scaladoc_comment_format_example', 'doc_format_example', 'example' }, s('scaladoc_comment_format_example', fmt([[
     === Example ===
     * {{{{
     * {}
     * }}}}
  ]], {
    i(0)
  }))),

  -- Scaladoc link
  sm({ 'scaladoc_comment_link', 'l' }, s('scaladoc_comment_link', fmt('[[{}]]', {
    i(1, 'stuff')
  }))),

  -- Scaladoc constructor
  sm({ 'scaladoc_constructor', 'constructor' }, s('scaladoc_constructor', fmt('@constructor {}', {
    i(0, 'description')
  }))),

  -- Scaladoc param
  sm({ 'scaladoc_param', 'param' }, s('scaladoc_param', fmt('@param {} {}', {
    i(1, 'paramName'),
    i(0, 'description')
  }))),

  -- Scaladoc group
  sm({ 'scaladoc_group', 'group' }, s('scaladoc_group', fmt('@group {}', {
    i(0, 'GroupName')
  }))),
}