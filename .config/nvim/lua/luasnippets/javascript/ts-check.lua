local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

-- Reference:
-- - https://docs.deno.com/runtime/fundamentals/typescript/

return list.concat(
  sm({ 'ts_check', 'enable_ts_check' }, t('// @ts-check')),

  {
    s(
      'document_comment',
      fmt(
        [[
      /**
       * {}
       */
    ]],
        {
          i(1, ''),
        }
      )
    ),
  },

  sm(
    { 'document_comment_oneline', 'docline' },
    fmt('/** {} */', {
      i(1, ''),
    })
  ),

  {
    s(
      'typedef',
      fmt(
        [[
      /**
       * @typedef {{{type}}} {name}
       */
    ]],
        {
          type = i(2, 'type'),
          name = i(1, 'name'),
        }
      )
    ),
  },

  sm(
    { 'property', 'prop' },
    fmt('@property {{{type}}} {name}', {
      type = i(2, 'type'),
      name = i(1, 'name'),
    })
  ),

  sm(
    { 'property_optional', 'opt_prop' },
    fmt('@property {{{type}}} [{name}]', {
      type = i(2, 'type'),
      name = i(1, 'name'),
    })
  ),

  {
    s(
      'type',
      fmt('/** @type {{{type}}} */', {
        type = i(1, 'type'),
      })
    ),
  },

  sm(
    { 'template', 'generic', 'generics' },
    fmt('@template {}', {
      i(1, 'T'),
    })
  ),

  sm(
    { 'template_default', 'generic_default', 'generics_default' },
    fmt('@template [{T}={Default}]', {
      T = i(1, 'T'),
      Default = i(2, 'Default'),
    })
  ),

  {
    s(
      'param',
      fmt('@param {name} {{{type}}}', {
        name = i(1, 'name'),
        type = i(2, 'type'),
      })
    ),
  },

  sm(
    { 'param_optional', 'opt', 'opt_param' },
    fmt('@param [{name}] {{{type}}}', {
      name = i(1, 'name'),
      type = i(2, 'type'),
    })
  ),

  sm(
    { 'returns', 'return' },
    fmt('@returns {{{type}}}', {
      type = i(1, 'type'),
    })
  ),

  {
    s('see', t('@see')),
  },

  sm(
    { 'link' },
    fmt('{{@link {}}}', {
      i(1, ''),
    })
  ),

  {
    s(
      'see_link',
      fmt('@see {{@link {}}}', {
        i(1, ''),
      })
    ),

    s('ts_expect_error', t('// @ts-expect-error')),
  },

  sm({ 'unk' }, t('unknown')),

  sm(
    { 'exhaustive', 'check_unreachable', 'satisfied' },
    fmt(
      [=[
    /** @type {{never}} */
    const satisfied = {var}
    throw new Error(`unreachable: ${{satisfied}}`)
  ]=],
      {
        var = i(1, 'var'),
      }
    )
  )
)
