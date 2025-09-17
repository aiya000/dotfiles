local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

return list.concat(
  -- Basic language features
  {
    s('use_strict', t("'use strict';")),
  },

  -- Import/Export
  sm({ 'import', 'imp' }, fmt("import {{ {} }} from '{}'", {
    i(1, 'objects'),
    i(2, 'module-name'),
  })),

  sm({ 'import_default', 'imd' }, fmt("import {} from '{}'", {
    i(1, 'object'),
    i(2, 'module-name'),
  })),

  sm({ 'import_as', 'import_qualified', 'imq' }, fmt("import * as {} from '{}'", {
    i(1, 'name'),
    i(2, 'module-name'),
  })),

  -- Control structures
  {
    s('for_traditional', fmt([[
      for (let {var}; {condition}; {effect}) {{
        {}
      }}
    ]], {
      var = i(1, 'i = 0'),
      condition = i(2, 'i < length'),
      effect = i(3, 'i++'),
      i(4, ''),
    })),

    s('if', fmt([[
      if ({condition}) {{
        {}
      }}
    ]], {
      condition = i(1, 'cond'),
      i(2, ''),
    })),

    s('else', fmt([[
      else {{
        {}
      }}
    ]], {
      i(1, ''),
    })),

    s('switch', fmt([[
      switch ({var}) {{
        {}
      }}
    ]], {
      var = i(1, 'var'),
      i(2, ''),
    })),

    s('case', fmt([[
      case {label}:
        {}
        break;
    ]], {
      label = i(1, 'LABEL'),
      i(2, ''),
    })),

    s('try', fmt([[
      try {{
        {}
      }}
    ]], {
      i(1, ''),
    })),

    s('catch', fmt([[
      catch (e) {{
        {}
      }}
    ]], {
      i(1, ''),
    })),
  },

  sm({ 'for_of', 'for', 'for_each' }, fmt([[
    for (const {x} of {xs}) {{
      {}
    }}
  ]], {
    x = i(1, 'x'),
    xs = i(2, 'xs'),
    i(3, ''),
  })),

  sm({ 'for_in', 'for_each_propeties' }, fmt([[
    for (const {x} in {xs}) {{
      {}
    }}
  ]], {
    x = i(1, 'x'),
    xs = i(2, 'xs'),
    i(3, ''),
  })),

  sm({ 'finally', 'fin', 'fina' }, fmt([[
    finally {{
      {}
    }}
  ]], {
    i(1, ''),
  })),

  -- Function definitions
  sm({ 'function', 'fun' }, fmt([[
    function {name}({args}) {{
      {}
    }}
  ]], {
    name = i(1, 'name'),
    args = i(2, 'args'),
    i(3, ''),
  })),

  sm({ 'arrow_function', 'arrow', 'ar' }, fmt('({args}) => {}', {
    args = i(1, 'args'),
    i(2, ''),
  })),

  sm({ 'method', 'met' }, fmt([[
    {name}({args}) {{
      {}
    }}
  ]], {
    name = i(1, 'name'),
    args = i(2, 'args'),
    i(3, ''),
  })),

  -- Classes
  {
    s('class', fmt([[
      class {name} {{
        {}
      }}
    ]], {
      name = i(1, 'Class'),
      i(2, ''),
    })),

    s('constructor', fmt([[
      constructor({args}) {{
        {}
      }}
    ]], {
      args = i(1, 'args'),
      i(2, ''),
    })),
  },

  -- Export
  sm({ 'export', 'exp' }, t('export')),

  {
    s('export_default', fmt([[
      export default {{
        {}
      }}
    ]], {
      i(1, ''),
    })),
  },

  -- Console and debugging
  sm({ 'print', 'println', 'pr' }, fmt('console.log({});', {
    i(1, 'hello'),
  })),

  {
    s('poi', fmt("console.log('poi:', {})", {
      i(1, ''),
    })),
  },

  -- Operators
  sm({ 'equal', 'eq' }, t('===')),
  sm({ 'not_equal', 'ne' }, t('!==')),

  -- Error handling
  sm({ 'throw_new_error', 'throw' }, fmt('throw new Error({})', {
    i(1, ''),
  })),

  -- Modern JavaScript
  sm({ 'const_function', 'cfun' }, fmt('const {name} = ({args}) => {}', {
    name = i(1, 'funcName'),
    args = i(2, 'args'),
    i(3, ''),
  }))
)