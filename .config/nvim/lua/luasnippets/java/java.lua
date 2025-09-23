local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = utils.s
local i = ls.insert_node
local t = ls.text_node

local java_snippets = list.concat(
  -- Syntaxes
  sm(
    { 'class', 'cla' },
    fmt(
      [[
    class {name} {{{body}}}
  ]],
      {
        name = i(1, 'Name'),
        body = i(2, ''),
      }
    )
  ),

  sm(
    { 'function', 'func', 'fun' },
    fmt(
      [[
    static {return_type} {func_name}({args}) {{{body}}}
  ]],
      {
        return_type = i(1, 'void'),
        func_name = i(2, '#:func'),
        args = i(3, '#:args'),
        body = i(4, ''),
      }
    )
  ),

  sm(
    { 'method', 'met' },
    fmt(
      [[
    {return_type} {method_name}({args}) {{{body}}}
  ]],
      {
        return_type = i(1, 'void'),
        method_name = i(2, '#:method'),
        args = i(3, '#:args'),
        body = i(4, ''),
      }
    )
  ),

  s(
    'try',
    fmt(
      [[
    try {{{body}}}
  ]],
      {
        body = i(1, ''),
      }
    )
  ),

  s(
    'catch',
    fmt(
      [[
    catch ({exception}) {{{body}}}
  ]],
      {
        exception = i(1, 'exception'),
        body = i(2, ''),
      }
    )
  ),

  s(
    'finally',
    fmt(
      [[
    finally {{{body}}}
  ]],
      {
        body = i(1, ''),
      }
    )
  ),

  s(
    'try_with_resource',
    fmt(
      [[
    try ({resource}) {{{body}}}
  ]],
      {
        resource = i(1, '#:resource'),
        body = i(2, ''),
      }
    )
  ),

  s(
    'if',
    fmt(
      [[
    if ({condition}) {{{body}}}
  ]],
      {
        condition = i(1, 'condition'),
        body = i(2, ''),
      }
    )
  ),

  s(
    'else',
    fmt(
      [[
    else {{{body}}}
  ]],
      {
        body = i(1, ''),
      }
    )
  ),

  sm(
    { 'for_each', 'for' },
    fmt(
      [[
    for ({type} {var} : {collection}) {{{body}}}
  ]],
      {
        type = i(1, 'type'),
        var = i(2, 'x'),
        collection = i(3, 'xs'),
        body = i(4, ''),
      }
    )
  ),

  sm(
    { 'for_with_index', 'for_traditional' },
    fmt(
      [[
    for ({init}; {condition}; {update}) {{{body}}}
  ]],
      {
        init = i(1, 'var'),
        condition = i(2, 'condition'),
        update = i(3, 'effect'),
        body = i(4, ''),
      }
    )
  ),

  s(
    'switch',
    fmt(
      [[
    switch ({var}) {{{body}}}
  ]],
      {
        var = i(1, 'var'),
        body = i(2, ''),
      }
    )
  ),

  s(
    'case',
    fmt(
      [[
    case {label}:{body}
  ]],
      {
        label = i(1, 'Label'),
        body = i(2, ''),
      }
    )
  ),

  s(
    'enum',
    fmt(
      [[
    enum {name} {{{body}}}
  ]],
      {
        name = i(1, 'Name'),
        body = i(2, ''),
      }
    )
  ),

  sm(
    { 'import', 'imp' },
    fmt('import {package};', {
      package = i(1, ''),
    })
  ),

  -- Templates
  s('public_static', t('public static')),

  sm(
    { 'public_function', 'pubfun' },
    fmt(
      [[
    public static {return_type} {method_name}({args}) {{{body}}}
  ]],
      {
        return_type = i(1, 'void'),
        method_name = i(2, '#:methodName'),
        args = i(3, '#:args'),
        body = i(4, ''),
      }
    )
  ),

  sm(
    { 'public_method', 'pubmet' },
    fmt(
      [[
    public {return_type} {method_name}({args}) {{{body}}}
  ]],
      {
        return_type = i(1, 'void'),
        method_name = i(2, '#:method'),
        args = i(3, '#:args'),
        body = i(4, ''),
      }
    )
  ),

  sm(
    { 'private_function', 'prifun' },
    fmt(
      [[
    private static {return_type} {method_name}({args}) {{{body}}}
  ]],
      {
        return_type = i(1, 'void'),
        method_name = i(2, '#:methodName'),
        args = i(3, '#:args'),
        body = i(4, ''),
      }
    )
  ),

  sm(
    { 'private_method', 'primet' },
    fmt(
      [[
    private {return_type} {method_name}({args}) {{{body}}}
  ]],
      {
        return_type = i(1, 'void'),
        method_name = i(2, '#:methodName'),
        args = i(3, '#:args'),
        body = i(4, ''),
      }
    )
  ),

  s(
    'override_to_string',
    fmt(
      [[
    @Override
    public String toString() {{{body}}}
  ]],
      {
        body = i(1, ''),
      }
    )
  ),

  s(
    'override_equals',
    fmt(
      [[
    @Override
    public boolean equals({param}) {{{body}}}
  ]],
      {
        param = i(1, 'X x'),
        body = i(2, ''),
      }
    )
  ),

  s(
    'main',
    fmt(
      [[
    public static void main(String[] args) {{{body}}}
  ]],
      {
        body = i(1, ''),
      }
    )
  ),

  s('public_static_final', t('public static final')),

  -- Annotation
  sm({ 'annotation_override', 'override', 'Override' }, t('@Override')),

  sm({ 'annotation_safe_varargs', 'safevarargs' }, t('@SafeVarargs')),

  sm(
    { 'annoration_suppress_warnings', 'suppress_warnings' },
    fmt(
      [[
    @SuppressWarnings("{warning}")
  ]],
      {
        warning = i(1, ''),
      }
    )
  ),

  sm({ 'annoration_suppress_warnings_serial', 'suppress_warnings_serial' }, t('@SuppressWarnings("serial")')),

  -- Expression
  sm(
    { 'println', 'pr' },
    fmt(
      [[
    System.out.println({content});
  ]],
      {
        content = i(1, ''),
      }
    )
  ),

  s(
    'print',
    fmt(
      [[
    System.out.print({content});
  ]],
      {
        content = i(1, ''),
      }
    )
  ),

  -- Other
  s(
    'precode',
    fmt(
      [[
    <pre><code>{content}</code></pre>
  ]],
      {
        content = i(1, ''),
      }
    )
  ),

  s('br', t('<br>'))
)

return {
  snippets = java_snippets,
  autosnippets = {},
}
