local fmt = require('luasnip.extras.fmt').fmt
local ls = require('luasnip')

local s = ls.snippet
local i = ls.insert_node

return {
  s(
    'test_describe',
    fmt(
      [[
    test.describe('{what}', () => {{{}}})
  ]],
      {
        what = i(1, 'what'),
        i(2, ''),
      }
    )
  ),

  s(
    'test_beforeAll',
    fmt(
      [[
    test.beforeAll(({{ {page} }}) => {{{}}})
  ]],
      {
        page = i(1, 'page'),
        i(2, ''),
      }
    )
  ),

  s(
    'test_beforeAll_async',
    fmt(
      [[
    test.beforeAll(async ({{ {page} }}) => {{{}}})
  ]],
      {
        page = i(1, 'page'),
        i(2, ''),
      }
    )
  ),

  s(
    'test_afterAll',
    fmt(
      [[
    test.afterAll(({{ {page} }}) => {{{}}})
  ]],
      {
        page = i(1, 'page'),
        i(2, ''),
      }
    )
  ),

  s(
    'test_afterAll_async',
    fmt(
      [[
    test.afterAll(async ({{ {page} }}) => {{{}}})
  ]],
      {
        page = i(1, 'page'),
        i(2, ''),
      }
    )
  ),

  s(
    'test_beforeEach',
    fmt(
      [[
    test.beforeEach(({{ {page} }}) => {{{}}})
  ]],
      {
        page = i(1, 'page'),
        i(2, ''),
      }
    )
  ),

  s(
    'test_beforeEach_async',
    fmt(
      [[
    test.beforeEach(async ({{ {page} }}) => {{{}}})
  ]],
      {
        page = i(1, 'page'),
        i(2, ''),
      }
    )
  ),

  s(
    'test_afterEach',
    fmt(
      [[
    test.afterEach(({{ {page} }}) => {{{}}})
  ]],
      {
        page = i(1, 'page'),
        i(2, ''),
      }
    )
  ),

  s(
    'test_afterEach_async',
    fmt(
      [[
    test.afterEach(async ({{ {page} }}) => {{{}}})
  ]],
      {
        page = i(1, 'page'),
        i(2, ''),
      }
    )
  ),
}
