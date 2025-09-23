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
      'logger_trace',
      fmt(
        [[
      {logger}.trace({})
    ]],
        {
          logger = i(1, 'logger'),
          i(2, ''),
        }
      )
    ),
  },

  sm(
    { 'logger_debug', 'log' },
    fmt(
      [[
    {logger}.debug({})
  ]],
      {
        logger = i(1, 'logger'),
        i(2, ''),
      }
    )
  ),

  {
    s(
      'logger_info',
      fmt(
        [[
      {logger}.info({})
    ]],
        {
          logger = i(1, 'logger'),
          i(2, ''),
        }
      )
    ),

    s(
      'logger_warn',
      fmt(
        [[
      {logger}.warn({})
    ]],
        {
          logger = i(1, 'logger'),
          i(2, ''),
        }
      )
    ),

    s(
      'logger_error',
      fmt(
        [[
      {logger}.error({})
    ]],
        {
          logger = i(1, 'logger'),
          i(2, ''),
        }
      )
    ),

    s(
      'logger_fatal',
      fmt(
        [[
      {logger}.fatal({})
    ]],
        {
          logger = i(1, 'logger'),
          i(2, ''),
        }
      )
    ),
  }
)
