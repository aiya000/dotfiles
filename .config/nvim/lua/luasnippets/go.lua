local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

return {
  sm({ 'package', 'pack' }, fmt('package {}', {
    i(1, 'packageName'),
  })),

  sm({ 'func', 'fun' }, fmt([[
    func {name}({args}){return_type} {{
        {}
    }}
  ]], {
    name = i(1, 'funcName'),
    args = i(2, ''),
    return_type = i(3, ''),
    i(4, 'return'),
  })),

  sm({ 'import', 'imp' }, fmt([[
    import (
        "{}"
    )
    {}
  ]], {
    i(1, 'fmt'),
    i(2, ''),
  })),

  s('if', fmt('if {} {{{}}}', {
    i(1, 'cond'),
    i(2, 'here'),
  })),

  s('else', fmt('else {{{}}}', {
    i(1, 'here'),
  })),

  s('array_type', fmt('[]{}', {
    i(1, 'TypeName'),
  })),

  sm({ 'array_constant', 'array' }, fmt('[]{}{{{}}}}', {
    i(1, 'TypeName'),
    i(2, 'here'),
  })),

  sm({ 'val', 'const' }, fmt('{} := {}', {
    i(1, 'varName'),
    i(2, ''),
  })),

  sm({ 'init_global_var', 'var' }, fmt('var {} = {}', {
    i(1, 'varName'),
    i(2, ''),
  })),

  sm({ 'var_array', 'var_slice' }, fmt('var {} [{}]{}', {
    i(1, 'varName'),
    i(2, 'size'),
    i(3, 'TypeName'),
  })),

  s('type', fmt('type {}', {
    i(1, 'Name'),
  })),

  sm({ 'struct', 'str', 'stru' }, fmt('struct {{{}}}', {
    i(1, 'here'),
  })),

  s('type_assert', fmt('{}, {} := {}.({}})', {
    i(1, 'extractedVarName'),
    i(2, 'isItExpectedType'),
    i(3, 'varName'),
    i(4, 'expectedType'),
  })),

  s('defer', fmt('defer {}', {
    i(1, 'expr'),
  })),

  s('for', fmt('for {}, {} := range {} {{{}}}', {
    i(1, '_'),
    i(2, 'name'),
    i(3, 'array'),
    i(4, 'here'),
  })),

  s('switch', fmt('switch {} {{{}}}', {
    i(1, 'x'),
    i(2, 'here'),
  })),

  s('case', fmt('case {}:{}', {
    i(1, 'pattern'),
    i(2, ''),
  })),

  -- Expressions
  s('fprint', fmt('fmt.Fprint({}, {})', {
    i(1, 'os.Stderr'),
    i(2, 'err'),
  })),

  s('fprintln', fmt('fmt.Fprintln({}, {})', {
    i(1, 'os.Stderr'),
    i(2, 'err'),
  })),

  sm({ 'println', 'pr' }, fmt('fmt.Println({})', {
    i(1, 'here'),
  })),

  sm({ 'log_println', 'log' }, fmt('log.Println({})', {
    i(1, 'here'),
  })),

  s('make', fmt('make([]{}, {})', {
    i(1, 'type'),
    i(2, 'len'),
  })),

  s('map_type', fmt('map[{}]{}', {
    i(1, 'KeyType'),
    i(2, 'ValueType'),
  })),

  s('make_map', fmt('make(map[{}]{})', {
    i(1, 'KeyType'),
    i(2, 'ValueType'),
  })),

  -- Others
  sm({ 'document_comment', 'doc' }, fmt([[
    /**
     * {}
     */
  ]], {
    i(1, ''),
  })),

  sm({ 'if_err', 'ife' }, fmt('if err != nil {{{}}}', {
    i(1, 'here'),
  })),

  s('append', fmt('{xs} = append({xs}, {})', {
    xs = i(1, 'xs'),
    i(2, ''),
  })),
}