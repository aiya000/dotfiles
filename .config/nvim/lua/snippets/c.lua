local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

return {
  s('for', fmt([[
    for ({var}; {condition}; {effect})
    {{
        {}
    }}
  ]], {
    var = i(1, 'var'),
    condition = i(2, 'condition'),
    effect = i(3, 'effect'),
    i(4, ''),
  })),

  s('if', fmt([[
    if ({condition})
    {{
        {}
    }}
  ]], {
    condition = i(1, 'condition'),
    i(2, ''),
  })),

  s('else', fmt([[
    else
    {{
        {}
    }}
  ]], {
    i(1, ''),
  })),

  s('switch', fmt([[
    switch ({target})
    {{
        {}
    }}
  ]], {
    target = i(1, 'target'),
    i(2, ''),
  })),

  s('while', fmt([[
    while ({condition})
    {{
        {}
    }}
  ]], {
    condition = i(1, 'condition'),
    i(2, ''),
  })),

  s('struct', fmt([[
    struct {name}
    {{
        {}
    }};
  ]], {
    name = i(1, 'name'),
    i(2, ''),
  })),

  s('typedef_struct', fmt([[
    typedef struct
    {{
        {}
    }} {name};
  ]], {
    i(1, ''),
    name = i(2, 'name'),
  })),

  sm({ 'function', 'fun' }, fmt([[
    {return_type}
    {name}({args})
    {{
        {}
    }}
  ]], {
    return_type = i(1, 'return_type'),
    name = i(2, 'name'),
    args = i(3, 'args'),
    i(4, ''),
  })),

  s('static_fun', fmt([[
    static {type}
    {name}({args})
    {{
        {}
    }}
  ]], {
    type = i(1, 'type'),
    name = i(2, 'name'),
    args = i(3, 'args'),
    i(4, ''),
  })),

  -- Macros
  sm({ 'include_library', 'include', 'import', 'imp' }, fmt('#include <{}>', {
    i(1, 'here'),
  })),

  sm({ 'include_path', 'includep' }, fmt('#include "{}"', {
    i(1, 'path'),
  })),

  -- Expressions
  s('printf', fmt('printf("{}"{}){};', {
    i(1, ''),
    i(2, ''),
    i(3, ''),
  })),

  sm({ 'printf_ln', 'pr' }, fmt('printf("{}\\n"{}){};', {
    i(1, ''),
    i(2, ''),
    i(3, ''),
  })),

  -- Others
  sm({ 'document_comment', 'doc' }, fmt([[
    /*
     * {}
     */
  ]], {
    i(1, ''),
  })),
}