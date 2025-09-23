local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

local android_snippets = {}

vim.list_extend(
  android_snippets,
  sm(
    { 'log_debug', 'logd' },
    fmt(
      [[
  Log.d({tag}, {message});
]],
      {
        tag = i(1, 'TAG'),
        message = i(2, ''),
      }
    )
  )
)

vim.list_extend(
  android_snippets,
  sm(
    { 'log_error', 'loge' },
    fmt(
      [[
  Log.e({tag}, {message});
]],
      {
        tag = i(1, 'TAG'),
        message = i(2, ''),
      }
    )
  )
)

vim.list_extend(
  android_snippets,
  sm(
    { 'log_verbose', 'logv' },
    fmt(
      [[
  Log.v({tag}, {message});
]],
      {
        tag = i(1, 'TAG'),
        message = i(2, ''),
      }
    )
  )
)

return {
  snippets = android_snippets,
  autosnippets = {},
}
