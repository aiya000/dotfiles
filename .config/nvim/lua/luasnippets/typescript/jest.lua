local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node

return list.concat(
  {
    s(
      'test',
      fmt(
        [[
      test('{does_something}', () => {{
        {}
      }})
    ]],
        {
          does_something = i(1, 'does_something'),
          i(2, ''),
        }
      )
    ),

    s(
      'test_async',
      fmt(
        [[
      test('{does_something}', async () => {{
        {}
      }})
    ]],
        {
          does_something = i(1, 'does_something'),
          i(2, ''),
        }
      )
    ),
  },

  sm(
    { 'describe', 'desc' },
    fmt(
      [[
    describe('{what}', () => {{
      {}
    }})
  ]],
      {
        what = i(1, 'what'),
        i(2, ''),
      }
    )
  ),

  {
    s(
      'describe_async',
      fmt(
        [[
      describe('{what}', async () => {{
        {}
      }})
    ]],
        {
          what = i(1, 'what'),
          i(2, ''),
        }
      )
    ),

    s(
      'it',
      fmt(
        [[
      it('{does_something}', () => {{
        {}
      }})
    ]],
        {
          does_something = i(1, 'does_something'),
          i(2, ''),
        }
      )
    ),

    s(
      'it_async',
      fmt(
        [[
      it('{does_something}', async () => {{
        {}
      }})
    ]],
        {
          does_something = i(1, 'does_something'),
          i(2, ''),
        }
      )
    ),

    s(
      'beforeAll',
      fmt(
        [[
      beforeAll(() => {{
        {}
      }})
    ]],
        {
          i(1, ''),
        }
      )
    ),

    s(
      'beforeAll_async',
      fmt(
        [[
      beforeAll(async () => {{
        {}
      }})
    ]],
        {
          i(1, ''),
        }
      )
    ),

    s(
      'afterAll',
      fmt(
        [[
      afterAll(() => {{
        {}
      }})
    ]],
        {
          i(1, ''),
        }
      )
    ),

    s(
      'afterAll_async',
      fmt(
        [[
      afterAll(async () => {{
        {}
      }})
    ]],
        {
          i(1, ''),
        }
      )
    ),

    s(
      'expect_toBe',
      fmt(
        [[
      expect({actual}).toBe({expected})
    ]],
        {
          actual = i(1, 'actual'),
          expected = i(2, 'expected'),
        }
      )
    ),

    s(
      'expect_toEqual',
      fmt(
        [[
      expect({actual}).toEqual({expected})
    ]],
        {
          actual = i(1, 'actual'),
          expected = i(2, 'expected'),
        }
      )
    ),
  }
)
