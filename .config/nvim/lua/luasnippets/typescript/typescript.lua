local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = utils.s
local i = ls.insert_node
local t = ls.text_node

return list.concat(
  -- Syntaxes
  sm(
    { 'for_of', 'for' },
    fmt(
      [[
    for (const {x} of {xs}) {{
      {body}
    }}
  ]],
      {
        x = i(1, 'x'),
        xs = i(2, 'xs'),
        body = i(3, ''),
      }
    )
  ),

  sm(
    { 'for_await', 'forawait', 'fora' },
    fmt(
      [[
    for await (const {x} of {xs}) {{
      {body}
    }}
  ]],
      {
        x = i(1, 'x'),
        xs = i(2, 'xs'),
        body = i(3, ''),
      }
    )
  ),

  {
    s(
      'for_in',
      fmt(
        [[
      for (const {key} in {obj}) {{
        {body}
      }}
    ]],
        {
          key = i(1, 'i'),
          obj = i(2, 'xs'),
          body = i(3, ''),
        }
      )
    ),

    s(
      'for_traditional',
      fmt(
        [[
      for ({init}; {condition}; {increment}) {{
        {body}
      }}
    ]],
        {
          init = i(1, 'let i = 0'),
          condition = i(2, 'i < x'),
          increment = i(3, 'i++'),
          body = i(4, ''),
        }
      )
    ),

    s(
      'while',
      fmt(
        [[
      while ({condition}) {{
        {body}
      }}
    ]],
        {
          condition = i(1, 'cond'),
          body = i(2, ''),
        }
      )
    ),

    s(
      'switch',
      fmt(
        [[
      switch ({expr}) {{
        {cases}
      }}
    ]],
        {
          expr = i(1, 'name'),
          cases = i(2, ''),
        }
      )
    ),

    s(
      'case',
      fmt('case {constant}: {statement}', {
        constant = i(1, 'constant'),
        statement = i(2, ''),
      })
    ),

    s('break', t('break')),
  },

  sm(
    { 'throw_new_error', 'throw' },
    fmt('throw new Error({})', {
      i(1, ''),
    })
  ),

  {
    s(
      'if',
      fmt(
        [[
      if ({condition}) {{
        {body}
      }}
    ]],
        {
          condition = i(1, 'cond'),
          body = i(2, ''),
        }
      )
    ),

    s(
      'else',
      fmt(
        [[
      else {{
        {body}
      }}
    ]],
        {
          body = i(1, ''),
        }
      )
    ),
  },

  sm(
    { 'function', 'fun' },
    fmt([[
      function {name}({}) {{
        {}
      }}
    ]], {
      name = i(1, 'name'),
      i(2, ''),
      i(3, ''),
    })
  ),

  sm(
    { 'async_function', 'afun' },
    fmt(
      [[
    async function {name}({args}) {{
      {}
    }}
  ]],
      {
        name = i(1, 'name'),
        args = i(2, 'args'),
        i(3, ''),
      }
    )
  ),

  sm(
    { 'generator_function', 'gfun' },
    fmt(
      [[
    function* {name}({args}) {{
      {}
    }}
  ]],
      {
        name = i(1, 'name'),
        args = i(2, 'args'),
        i(3, ''),
      }
    )
  ),

  sm(
    { 'async_generator_function', 'agfun' },
    fmt(
      [[
    async function* {name}({args}) {{
      {}
    }}
  ]],
      {
        name = i(1, 'name'),
        args = i(2, 'args'),
        i(3, ''),
      }
    )
  ),

  sm(
    { 'arrow_function', 'ar' },
    fmt('({args}) => {}', {
      args = i(1, 'args'),
      i(2, ''),
    })
  ),

  sm(
    { 'async_arrow_function', 'aar' },
    fmt('async ({args}) => {}', {
      args = i(1, 'args'),
      i(2, ''),
    })
  ),

  sm(
    { 'method', 'met' },
    fmt(
      [[
    {name}({args}) {{
      {}
    }}
  ]],
      {
        name = i(1, 'name'),
        args = i(2, 'args'),
        i(3, ''),
      }
    )
  ),

  sm(
    { 'interface', 'inter' },
    fmt(
      [[
    interface {name} {{
      {body}
    }}
  ]],
      {
        name = i(1, 'Name'),
        body = i(2, ''),
      }
    )
  ),

  {
    s(
      'type',
      fmt('type {name} = {definition}', {
        name = i(1, 'Name'),
        definition = i(2, ''),
      })
    ),

    s(
      'try',
      fmt(
        [[
      try {{
        {body}
      }}
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
      catch (e) {{
        {body}
      }}
    ]],
        {
          body = i(1, ''),
        }
      )
    ),
  },

  sm(
    { 'finally', 'fin', 'fina' },
    fmt(
      [[
    finally {{
      {body}
    }}
  ]],
      {
        body = i(1, ''),
      }
    )
  ),

  sm(
    { 'import_as', 'imq' },
    fmt("import * as {alias} from '{module}'", {
      alias = i(1, ''),
      module = i(2, ''),
    })
  ),

  sm(
    { 'import', 'imp' },
    fmt("import {{ {imports} }} from '{module}'", {
      imports = i(1, ''),
      module = i(2, ''),
    })
  ),

  sm(
    { 'import_default_export', 'import_default', 'imd' },
    fmt("import {name} from '{module}'", {
      name = i(1, ''),
      module = i(2, ''),
    })
  ),

  sm({ 'export', 'exp' }, t('export')),

  sm(
    { 'export_from', 're_export' },
    fmt("export {{ {exports} }} from '{module}'", {
      exports = i(1, 'stuff'),
      module = i(2, 'module'),
    })
  ),

  {
    s(
      'const',
      fmt('const {name} = {value}', {
        name = i(1, 'x'),
        value = i(2, ''),
      })
    ),

    s(
      'let',
      fmt('let {name} = {value}', {
        name = i(1, 'x'),
        value = i(2, ''),
      })
    ),

    s('instanceof', t('instanceof')),
    s('typeof', t('typeof')),
    s('public', t('public')),
    s('private', t('private')),
    s('readonly', t('readonly')),
  },

  {
    s(
      'enum',
      fmt(
        [[
      enum {name} {{
        {body}
      }}
    ]],
        {
          name = i(1, 'Name'),
          body = i(2, ''),
        }
      )
    ),
  },

  sm(
    { 'class', 'cla' },
    fmt(
      [[
    class {name} {{
      {body}
    }}
  ]],
      {
        name = i(1, 'Name'),
        body = i(2, ''),
      }
    )
  ),

  {
    s(
      'constructor',
      fmt(
        [[
      constructor({args}) {{
        {body}
      }}
    ]],
        {
          args = i(1, 'args'),
          body = i(2, ''),
        }
      )
    ),

    s(
      'namespace',
      fmt(
        [[
      namespace {name} {{
        {body}
      }}
    ]],
        {
          name = i(1, 'Name'),
          body = i(2, ''),
        }
      )
    ),

    s(
      'module',
      fmt(
        [[
      module {name} {{
        {body}
      }}
    ]],
        {
          name = i(1, 'Name'),
          body = i(2, ''),
        }
      )
    ),
  },

  sm({ 'declare', 'decl' }, t('declare')),

  {
    s(
      'get',
      fmt(
        [[
      get {name}(): {type} {{
        {body}
      }}
    ]],
        {
          name = i(1, 'fieldName'),
          type = i(2, 'type'),
          body = i(3, ''),
        }
      )
    ),

    s(
      'set',
      fmt(
        [[
      set {name}(x: {type}) {{
        {body}
      }}
    ]],
        {
          name = i(1, 'fieldName'),
          type = i(2, 'type'),
          body = i(3, ''),
        }
      )
    ),
  },

  sm({ 'satisfies', 'sat' }, t('satisfies')),

  -- Expressions
  sm(
    { 'print', 'pr' },
    fmt('console.log({})', {
      i(1, ''),
    })
  ),

  sm(
    { 'print_error', 'pre' },
    fmt('console.error({})', {
      i(1, ''),
    })
  ),

  {
    s(
      'todo',
      fmt("throw new Error('TODO ({location})')", {
        location = i(1, 'somewhere'),
      })
    ),
  },

  -- Others
  {
    s(
      'reference',
      fmt("/// <reference path='{path}'/>", {
        path = i(1, 'd-ts-path'),
      })
    ),
  },

  -- Templates
  sm(
    { 'private_method', 'primet' },
    fmt(
      [[
    private {name}({args}): {returnType} {{
      {body}
    }}
  ]],
      {
        name = i(1, 'name'),
        args = i(2, 'args'),
        returnType = i(3, 'void'),
        body = i(4, ''),
      }
    )
  ),

  sm(
    { 'public_method', 'pubmet' },
    fmt(
      [[
    public {name}({args}): {returnType} {{
      {body}
    }}
  ]],
      {
        name = i(1, 'name'),
        args = i(2, 'args'),
        returnType = i(3, 'void'),
        body = i(4, ''),
      }
    )
  ),

  sm(
    { 'async_method', 'amet' },
    fmt(
      [[
    async {name}({args}): Promise<{returnType}> {{
      {body}
    }}
  ]],
      {
        name = i(1, 'name'),
        args = i(2, 'args'),
        returnType = i(3, 'void'),
        body = i(4, ''),
      }
    )
  ),

  sm(
    { 'public_async_method', 'pubamet' },
    fmt(
      [[
    public async {name}({args}): Promise<{returnType}> {{
      {body}
    }}
  ]],
      {
        name = i(1, 'name'),
        args = i(2, 'args'),
        returnType = i(3, 'void'),
        body = i(4, ''),
      }
    )
  ),

  sm(
    { 'private_async_method', 'priamet' },
    fmt(
      [[
    private async {name}({args}): Promise<{returnType}> {{
      {body}
    }}
  ]],
      {
        name = i(1, 'name'),
        args = i(2, 'args'),
        returnType = i(3, 'void'),
        body = i(4, ''),
      }
    )
  ),

  sm(
    { 'public_static_async_method', 'pubsamet' },
    fmt(
      [[
    public static async {name}({args}): Promise<{returnType}> {{
      {body}
    }}
  ]],
      {
        name = i(1, 'name'),
        args = i(2, 'args'),
        returnType = i(3, 'void'),
        body = i(4, ''),
      }
    )
  ),

  sm(
    { 'private_static_async_method', 'prisamet' },
    fmt(
      [[
    private static async {name}({args}): Promise<{returnType}> {{
      {body}
    }}
  ]],
      {
        name = i(1, 'name'),
        args = i(2, 'args'),
        returnType = i(3, 'void'),
        body = i(4, ''),
      }
    )
  ),

  sm({ 'public_readonly', 'pubre' }, t('public readonly')),
  sm({ 'private_readonly', 'prire' }, t('private readonly')),

  {
    s('pub', t('public')),
    s('pri', t('private')),
  },

  sm({ 'equals', 'eq' }, t('===')),
  sm({ 'not_equals', 'ne' }, t('!==')),

  sm(
    { 'public_get', 'pubget' },
    fmt(
      [[
    public get {name}(): {type} {{
      {body}
    }}
  ]],
      {
        name = i(1, 'fieldName'),
        type = i(2, 'type'),
        body = i(3, ''),
      }
    )
  ),

  sm(
    { 'public_set', 'pubset' },
    fmt(
      [[
    public set {name}(x: {type}) {{
      {body}
    }}
  ]],
      {
        name = i(1, 'fieldName'),
        type = i(2, 'type'),
        body = i(3, ''),
      }
    )
  ),

  sm(
    { 'private_get', 'priget' },
    fmt(
      [[
    private get {name}(): {type} {{
      {body}
    }}
  ]],
      {
        name = i(1, 'fieldName'),
        type = i(2, 'type'),
        body = i(3, ''),
      }
    )
  ),

  sm(
    { 'private_set', 'priset' },
    fmt(
      [[
    private set {name}(x: {type}) {{
      {body}
    }}
  ]],
      {
        name = i(1, 'fieldName'),
        type = i(2, 'type'),
        body = i(3, ''),
      }
    )
  ),

  {
    s(
      'property_method',
      fmt(
        [[
      {name}({args}): {returnType} {{
        {body}
      }}
    ]],
        {
          name = i(1, 'name'),
          args = i(2, 'args'),
          returnType = i(3, 'void'),
          body = i(4, ''),
        }
      )
    ),

    s(
      'tuple_type_to_union_type',
      fmt('{tuple}[number]', {
        tuple = i(1, 'tuple'),
      })
    ),
  },

  {
    -- Maps Type.K of all Type props to Type[K]
    s(
      'mapped_type',
      fmt('[{key} in {keyType}]: {valueType}', {
        key = i(2, 'K'),
        keyType = i(1, 'keyof Type'),
        valueType = i(3, 'Type[K]'),
      })
    ),

    s(
      'mapped_type_object',
      fmt('{{ [{key} in {keyType}]: {valueType} }}', {
        key = i(2, 'K'),
        keyType = i(1, 'keyof Type'),
        valueType = i(3, 'Type[K]'),
      })
    ),
  },

  -- Expressions
  sm({ 'debugger', 'break_point' }, t('debugger')),

  {
    s(
      'new_promise',
      fmt('new Promise((resolve, reject) => {})', {
        i(1, ''),
      })
    ),

    s('sleep_promise', t('const sleep = msec => new Promise(resolve => setTimeout(resolve, msec))')),

    s('regex_email_validation', t("/^[a-zA-Z0-9.!#$%&'*+/=?^_`{{|}}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*$/")),
  },

  -- Syntax templates
  sm(
    { 'const_function', 'cfun' },
    fmt('const {name} = ({args}) => {}', {
      name = i(1, 'name'),
      args = i(2, 'args'),
      i(3, ''),
    })
  ),

  sm(
    { 'const_arrow_function', 'acfun', 'cafun' },
    fmt('const {name} = async ({args}) => {}', {
      name = i(1, 'funcName'),
      args = i(2, 'args'),
      i(3, ''),
    })
  ),

  sm({ 'export_default', 'exd' }, t('export default')),

  -- Others
  sm(
    { 'block_typescript', 'blts' },
    fmt(
      [[
    ```typescript
    {code}
    ```
  ]],
      {
        code = i(1, 'here'),
      }
    )
  ),

  {
    s('ts_expect_error', t('// @ts-expect-error')),
  },

  sm(
    { 'console_log_poi', 'poi' },
    fmt("console.log('poi:', {})", {
      i(1, ''),
    })
  ),

  sm(
    { 'exhaustive', 'check_unreachable', 'satisfied', 'unreachable' },
    fmt(
      [[
      throw new Error(`unreachable: ${{{value}}} satisfies never`)
    ]],
      {
        value = i(1, 'value'),
      }
    )
  )
)
